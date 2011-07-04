{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterADSP21xx.pas, released 2000-04-17.
The Original Code is based on the wbADSP21xxSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Wynand Breytenbach.
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

$Id: SynHighlighterADSP21xx.pas,v 1.17 2005/01/28 16:53:20 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a ADSP21xx highlighter for SynEdit)
@author(Wynand Breytenbach, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterADSP21xx unit provides a ADSP21xx DSP assembler highlighter for SynEdit.
}

{$IFNDEF QSYNHIGHLIGHTERADSP21XX}
unit SynHighlighterADSP21xx;
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
  TTokenKind = (tkComment, tkCondition, tkIdentifier, tkKey, tkNull, tkNumber,
    tkRegister, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnKnown, rsPascalComment, rsCComment, rsHexNumber,
    rsBinaryNumber, rsInclude);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TTokenKind of object;

  TSynADSP21xxSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..191] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    FTokenID: TTokenKind;
    fNumberAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fRegisterAttri: TSynHighlighterAttributes;
    fConditionAttri: TSynHighlighterAttributes;
    fNullAttri: TSynHighlighterAttributes;
    fUnknownAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func2: TTokenKind;
    function Func4: TTokenKind;
    function Func7: TTokenKind;
    function Func8: TTokenKind;
    function Func9: TTokenKind;
    function Func12: TTokenKind;
    function Func13: TTokenKind;
    function Func15: TTokenKind;
    function Func17: TTokenKind;
    function Func18: TTokenKind;
    function Func19: TTokenKind;
    function Func20: TTokenKind;
    function Func21: TTokenKind;
    function Func22: TTokenKind;
    function Func23: TTokenKind;
    function Func24: TTokenKind;
    function Func25: TTokenKind;
    function Func26: TTokenKind;
    function Func27: TTokenKind;
    function Func28: TTokenKind;
    function Func29: TTokenKind;
    function Func30: TTokenKind;
    function Func31: TTokenKind;
    function Func32: TTokenKind;
    function Func33: TTokenKind;
    function Func35: TTokenKind;
    function Func36: TTokenKind;
    function Func37: TTokenKind;
    function Func38: TTokenKind;
    function Func39: TTokenKind;
    function Func40: TTokenKind;
    function Func41: TTokenKind;
    function Func42: TTokenKind;
    function Func43: TTokenKind;
    function Func44: TTokenKind;
    function Func45: TTokenKind;
    function Func46: TTokenKind;
    function Func47: TTokenKind;
    function Func49: TTokenKind;
    function Func50: TTokenKind;
    function Func52: TTokenKind;
    function Func53: TTokenKind;
    function Func54: TTokenKind;
    function Func55: TTokenKind;
    function Func57: TTokenKind;
    function Func58: TTokenKind;
    function Func60: TTokenKind;
    function Func61: TTokenKind;
    function Func62: TTokenKind;
    function Func63: TTokenKind;
    function Func64: TTokenKind;
    function Func65: TTokenKind;
    function Func66: TTokenKind;
    function Func67: TTokenKind;
    function Func68: TTokenKind;
    function Func69: TTokenKind;
    function Func70: TTokenKind;
    function Func71: TTokenKind;
    function Func72: TTokenKind;
    function Func73: TTokenKind;
    function Func74: TTokenKind;
    function Func75: TTokenKind;
    function Func76: TTokenKind;
    function Func79: TTokenKind;
    function Func80: TTokenKind;
    function Func81: TTokenKind;
    function Func82: TTokenKind;
    function Func83: TTokenKind;
    function Func84: TTokenKind;
    function Func88: TTokenKind;
    function Func89: TTokenKind;
    function Func90: TTokenKind;
    function Func92: TTokenKind;
    function Func94: TTokenKind;
    function Func95: TTokenKind;
    function Func98: TTokenKind;
    function Func99: TTokenKind;
    function Func107: TTokenKind;
    function Func113: TTokenKind;
    function Func145: TTokenKind;
    function AltFunc: TTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TTokenKind;
    procedure MakeMethodTables;
    procedure PascalCommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CCommentProc;
    procedure CRProc;
    procedure ExclamationProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure IncludeCloseProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure BinaryNumber;
    procedure HexNumber;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TTokenKind;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property IdentChars;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property ConditionAttri: TSynHighlighterAttributes read fConditionAttri
      write fConditionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property RegisterAttri: TSynHighlighterAttributes read fRegisterAttri
      write fRegisterAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  Windows,
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  i, J: Char;
begin
  for i := #0 to #255 do
  begin
    Case i of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[i] := True;
      else Identifiers[i] := False;
    end;
    J := UpCase(i);
    Case i of
      '0'..'9','a'..'z', 'A'..'Z', '_': mHashTable[i] := Ord(J) - 64;
      else mHashTable[Char(i)] := 0;
    end;
  end;
end;

procedure TSynADSP21xxSyn.InitIdent;
var
  i: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;

  fIdentFuncTable[2] := Func2;
  fIdentFuncTable[4] := Func4;
  fIdentFuncTable[7] := Func7;
  fIdentFuncTable[8] := Func8;
  fIdentFuncTable[9] := Func9;
  fIdentFuncTable[12] := Func12;
  fIdentFuncTable[13] := Func13;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
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
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
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
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[145] := Func145;
end;

function TSynADSP21xxSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  while ToHash^ in ['_', '0'..'9'] do
    inc(ToHash);
  fStringLen := ToHash - fToIdent;
end;

function TSynADSP21xxSyn.KeyComp(const aKey: string): Boolean;
var
  i: Integer;
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

function TSynADSP21xxSyn.Func2: TTokenKind;
begin
  if KeyComp('b') then
  begin
    if FLine[Run+1] = '#'then
    begin
      Result := tkNumber;
      fRange := rsBinaryNumber;
    end
    else
    begin
      Result := tkIdentifier;
    end
  end
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func4: TTokenKind;
begin
  if KeyComp('ac') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func7: TTokenKind;
begin
  if KeyComp('af') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func8: TTokenKind;
begin
  if KeyComp('h') then
  begin
    if FLine[Run+1] = '#'then
    begin
      Result := tkNumber;
      fRange := rsHexNumber;
    end
    else
    begin
      Result := tkIdentifier;
    end
  end
  else if KeyComp('ce') then Result := tkCondition else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func9: TTokenKind;
begin
  if KeyComp('i0') then Result := tkRegister
  else if KeyComp('i1') then Result := tkRegister
  else if KeyComp('i2') then Result := tkRegister
  else if KeyComp('i3') then Result := tkRegister
  else if KeyComp('i4') then Result := tkRegister
  else if KeyComp('i5') then Result := tkRegister
  else if KeyComp('i6') then Result := tkRegister
  else if KeyComp('i7') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func12: TTokenKind;
begin
  if KeyComp('l0') then Result := tkRegister
  else if KeyComp('l1') then Result := tkRegister
  else if KeyComp('l2') then Result := tkRegister
  else if KeyComp('l3') then Result := tkRegister
  else if KeyComp('l4') then Result := tkRegister
  else if KeyComp('l5') then Result := tkRegister
  else if KeyComp('l6') then Result := tkRegister
  else if KeyComp('l7') then Result := tkRegister
  else if KeyComp('ge') then Result := tkCondition
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func13: TTokenKind;
begin
  if KeyComp('m0') then Result := tkRegister
  else if KeyComp('m1') then Result := tkRegister
  else if KeyComp('m2') then Result := tkRegister
  else if KeyComp('m3') then Result := tkRegister
  else if KeyComp('m4') then Result := tkRegister
  else if KeyComp('m5') then Result := tkRegister
  else if KeyComp('m6') then Result := tkRegister
  else if KeyComp('m7') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func15: TTokenKind;
begin
  if KeyComp('If') then Result := tkKey
  else if KeyComp('bm') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func17: TTokenKind;
begin
  if KeyComp('dm') then Result := tkRegister
  else if KeyComp('le') then Result := tkCondition
  else if KeyComp('hi') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func18: TTokenKind;
begin
  if KeyComp('ifc') then Result := tkRegister
  else if KeyComp('fl0') then Result := tkRegister
  else if KeyComp('fl1') then Result := tkRegister
  else if KeyComp('fl2') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func19: TTokenKind;
begin
  if KeyComp('Do') then Result := tkKey
  else if KeyComp('and') then Result := tkKey
  else if KeyComp('ne') then Result := tkCondition
  else if KeyComp('mf') then Result := tkCondition
  else if KeyComp('pc') then Result := tkRegister
  else if KeyComp('ar') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func20: TTokenKind;
begin
  if KeyComp('ena') then Result := tkKey
  else if KeyComp('cache') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func21: TTokenKind;
begin
  if KeyComp('Of') then Result := tkKey
  else if KeyComp('sb') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func22: TTokenKind;
begin
  if KeyComp('abs') then Result := tkRegister
  else if KeyComp('eq') then Result := tkCondition
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func23: TTokenKind;
begin
  if KeyComp('In') then Result := tkKey
  else if KeyComp('av') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func24: TTokenKind;
begin
  if KeyComp('io') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func25: TTokenKind;
begin
  if KeyComp('ax0') then Result := tkRegister
  else if KeyComp('ax1') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func26: TTokenKind;
begin
  if KeyComp('ay0') then Result := tkRegister
  else if KeyComp('ay1') then Result := tkRegister
  else if KeyComp('neg') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func27: TTokenKind;
begin
  if KeyComp('by') then Result := tkKey
  else if KeyComp('gt') then Result := tkCondition
  else if KeyComp('lo') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func28: TTokenKind;
begin
  if KeyComp('call') then Result := tkKey
  else if KeyComp('si') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func29: TTokenKind;
begin
  if KeyComp('pm') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func30: TTokenKind;
begin
  if KeyComp('idle') then Result := tkKey
  else if KeyComp('ifdef') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func31: TTokenKind;
begin
  if KeyComp('seg') then Result := tkRegister
  else if KeyComp('mr') then Result := tkRegister
  else if KeyComp('mr0') then Result := tkRegister
  else if KeyComp('mr1') then Result := tkRegister
  else if KeyComp('mr2') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func32: TTokenKind;
begin
  if KeyComp('ram') then Result := tkRegister
  else if KeyComp('lt') then Result := tkCondition
  else if KeyComp('dis') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func33: TTokenKind;
begin
  if KeyComp('Or') then Result := tkKey
  else if KeyComp('circ') then Result := tkRegister
  else if KeyComp('name') then Result := tkKey
  else if KeyComp('clr') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func35: TTokenKind;
begin
  if KeyComp('mv') then Result := tkCondition else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func36: TTokenKind;
begin
  if KeyComp('rnd') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func37: TTokenKind;
begin
  if KeyComp('mx0') then Result := tkRegister
  else if KeyComp('mx1') then Result := tkRegister
  else if KeyComp('sr') then Result := tkRegister
  else if KeyComp('sr0') then Result := tkRegister
  else if KeyComp('sr1') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func38: TTokenKind;
begin
  if KeyComp('my0') then Result := tkRegister
  else if KeyComp('my1') then Result := tkRegister
  else if KeyComp('ss') then Result := tkCondition
  else if KeyComp('endif') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func39: TTokenKind;
begin
  if KeyComp('for') then Result := tkKey
  else if KeyComp('shl') then Result := tkKey
  else if KeyComp('clear') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func40: TTokenKind;
begin
  if KeyComp('sat') then Result := tkKey
  else if KeyComp('su') then Result := tkRegister
  else if KeyComp('us') then Result := tkCondition
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func41: TTokenKind;
begin
  if KeyComp('var') then Result := tkKey
  else if KeyComp('else') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func42: TTokenKind;
begin
  if KeyComp('emode') then
    Result := tkRegister
  else if KeyComp('rx0') then
    Result := tkRegister
  else if KeyComp('rx1') then
    Result := tkRegister
  else if KeyComp('uu') then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func43: TTokenKind;
begin
  if KeyComp('local') then Result := tkKey
  else if KeyComp('define') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func44: TTokenKind;
begin
  if KeyComp('set') then Result := tkKey
  else if KeyComp('tx0') then Result := tkRegister
  else if KeyComp('tx1') then Result := tkRegister
  else if KeyComp('ifndef') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func45: TTokenKind;
begin
  if KeyComp('Shr') then Result := tkKey
  else if KeyComp('nop') then Result := tkKey
  else if KeyComp('exp') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func46: TTokenKind;
begin
  if KeyComp('rom') then Result := tkKey
  else if KeyComp('aux') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func47: TTokenKind;
begin
  if KeyComp('rti') then Result := tkKey
  else if KeyComp('pop') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func49: TTokenKind;
begin
  if KeyComp('cntl') then Result := tkKey
  else if KeyComp('global') then Result := tkKey
  else if KeyComp('not') then Result := tkCondition
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func50: TTokenKind;
begin
  if KeyComp('macro') then Result := tkKey
  else if KeyComp('undef') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func52: TTokenKind;
begin
  if KeyComp('init') then Result := tkKey
  else if KeyComp('boot') then Result := tkKey
  else if KeyComp('divq') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func53: TTokenKind;
begin
  if KeyComp('imask') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func54: TTokenKind;
begin
  if KeyComp('divs') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func55: TTokenKind;
begin
  if KeyComp('endmod') then Result := tkKey
  else if KeyComp('cntr') then Result := tkRegister
  else if KeyComp('pass') then Result := tkKey
  else if KeyComp('trap') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func57: TTokenKind;
begin
  if KeyComp('xor') then Result := tkKey
  else if KeyComp('rts') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func58: TTokenKind;
begin
  if KeyComp('icntl') then Result := tkRegister
  else if KeyComp('loop') then Result := tkKey
  else if KeyComp('regbank') then Result := tkKey
  else if KeyComp('sts') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func60: TTokenKind;
begin
  if KeyComp('jump') then Result := tkKey
  else if KeyComp('expadj') then Result := tkKey
  else if KeyComp('norm') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func61: TTokenKind;
begin
  if KeyComp('astat') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func62: TTokenKind;
begin
  if KeyComp('shift') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func63: TTokenKind;
begin
  if KeyComp('ashift') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func64: TTokenKind;
begin
  if KeyComp('push') then Result := tkKey
  else if KeyComp('test') then Result := tkKey
  else if KeyComp('true') then Result := tkKey
  else if KeyComp('clrbit') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func65: TTokenKind;
begin
  if KeyComp('timer') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func66: TTokenKind;
begin
  if KeyComp('toggle') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func67: TTokenKind;
begin
  if KeyComp('reset') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func68: TTokenKind;
begin
  if KeyComp('include') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func69: TTokenKind;
begin
  if KeyComp('port') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func70: TTokenKind;
begin
  if KeyComp('module') then Result := tkKey
  else if KeyComp('tglbit') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func71: TTokenKind;
begin
  if KeyComp('const') then Result := tkKey
  else if KeyComp('newpage') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func72: TTokenKind;
begin
  if KeyComp('static') then Result := tkKey
  else if KeyComp('modify') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func73: TTokenKind;
begin
  if KeyComp('endmacro') then Result := tkKey
  else if KeyComp('mstat') then Result := tkRegister
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func74: TTokenKind;
begin
  if KeyComp('lshift') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func75: TTokenKind;
begin
  if KeyComp('setbit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func76: TTokenKind;
begin
  if KeyComp('Until') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func79: TTokenKind;
begin
  if KeyComp('sstat') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func80: TTokenKind;
begin
  if KeyComp('flag_in') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func81: TTokenKind;
begin
  if KeyComp('m_mode') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func82: TTokenKind;
begin
  if KeyComp('entry') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func83: TTokenKind;
begin
  if KeyComp('segment') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func84: TTokenKind;
begin
  if KeyComp('abstract') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func88: TTokenKind;
begin
  if KeyComp('sec_reg') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func89: TTokenKind;
begin
  if KeyComp('forever') then Result := tkCondition else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func90: TTokenKind;
begin
  if KeyComp('ar_sat') then Result := tkKey
  else if KeyComp('go_mode') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func92: TTokenKind;
begin
  if KeyComp('DMOVLAY') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func94: TTokenKind;
begin
  if KeyComp('alt_reg') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func95: TTokenKind;
begin
  if KeyComp('testbit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func98: TTokenKind;
begin
  if KeyComp('av_latch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func99: TTokenKind;
begin
  if KeyComp('external') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func107: TTokenKind;
begin
  if KeyComp('bit_rev') then Result := tkKey else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func113: TTokenKind;
begin
  if KeyComp('flag_out') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.Func145: TTokenKind;
begin
  if KeyComp('topofpcstack') then Result := tkRegister else Result := tkIdentifier;
end;

function TSynADSP21xxSyn.AltFunc: TTokenKind;
begin
  Result := tkIdentifier
end;

function TSynADSP21xxSyn.IdentKind(MayBe: PChar): TTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 192 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynADSP21xxSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0: fProcTable[i] := NullProc;
      #10: fProcTable[i] := LFProc;
      #13: fProcTable[i] := CRProc;
      #1..#9, #11, #12, #14..#32: fProcTable[i] := SpaceProc;
      '$': fProcTable[i] := IntegerProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9': fProcTable[i] := NumberProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[i] := IdentProc;
      '{': fProcTable[i] := BraceOpenProc;
      '}': fProcTable[i] := BraceCloseProc;
      '/': fProcTable[i] := SlashProc;
      '>': fProcTable[i] := IncludeCloseProc;
      '!': fProcTable[i] := ExclamationProc;
      else fProcTable[i] := UnknownProc;
    end;
end;

constructor TSynADSP21xxSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.ForeGround := clTeal;
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.ForeGround := clOlive;
  AddAttribute(fNumberAttri);

  fRegisterAttri := TSynHighlighterAttributes.Create(SYNS_AttrRegister);
  fRegisterAttri.ForeGround := clBlue;
  AddAttribute(fRegisterAttri);

  fConditionAttri := TSynHighlighterAttributes.Create(SYNS_AttrCondition);
  fConditionAttri.ForeGround := clFuchsia;
  AddAttribute(fConditionAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);

  fNullAttri := TSynHighlighterAttributes.Create(SYNS_AttrNull);
  AddAttribute(fNullAttri);

  fUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord);
  AddAttribute(fUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterADSP21xx;
end;

procedure TSynADSP21xxSyn.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  Next;
end;

procedure TSynADSP21xxSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynADSP21xxSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynADSP21xxSyn.PascalCommentProc;
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
      '}':
        begin
          fRange := rsUnKnown;
          inc(Run);
          break;
        end;
      #10: break;
      #13: break;
      else inc(Run);
    end;
end;

procedure TSynADSP21xxSyn.CCommentProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0: begin
          NullProc;
          exit;
        end;
    #10:begin
         LFProc;
         exit;
        end;
    #13:begin
          CRProc;
          exit;
        end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        begin
          if FLine[Run+1] = '/' then
          begin
            fRange := rsUnknown;
            inc(Run, 2);
            break;
          end
          else
            Inc(Run);
        end;
      #10: break;
      #13: break;
      else inc(Run);
    end;
end;

procedure TSynADSP21xxSyn.BraceOpenProc;
begin
  fTokenID := tkComment;
  fRange := rsPascalComment;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          fRange := rsUnKnown;
          inc(Run);
          break;
        end;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;


procedure TSynADSP21xxSyn.IncludeCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynADSP21xxSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynADSP21xxSyn.ExclamationProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynADSP21xxSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    inc(Run);
end;

procedure TSynADSP21xxSyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
end;

procedure TSynADSP21xxSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynADSP21xxSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynADSP21xxSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f','x','X','.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynADSP21xxSyn.HexNumber;
begin
  inc(Run);
  fTokenID := tkNumber;
  fRange := rsUnKnown;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
  begin
    inc(Run);
  end;
end;

procedure TSynADSP21xxSyn.BinaryNumber;
begin
  inc(Run);
  fRange := rsUnKnown;
  while FLine[Run] in ['0'..'1'] do
  begin
    inc(Run);
  end;
  if FLine[Run] in ['2'..'9', 'A'..'F', 'a'..'f'] then
    fTokenID := tkIdentifier
  else
    fTokenID := tkNumber;
end;

procedure TSynADSP21xxSyn.SlashProc;
begin
  if FLine[Run + 1] = '*' then
  begin
    fTokenID := tkComment;
    fRange := rsCComment;
    inc(Run, 2);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        '*':  begin
                if FLine[Run+1] = '/' then
                begin
                  inc(Run, 2);
                  fRange := rsUnknown;
                  break;
                end
                else inc(Run);
              end;
        #10: break;
        #13: break;
        else inc(Run);
      end;
    end
  else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynADSP21xxSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynADSP21xxSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynADSP21xxSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsPascalComment: PascalCommentProc;
    rsCComment: CCommentProc;
    rsHexNumber: HexNumber;
    rsBinaryNumber: BinaryNumber;
  else
    fRange := rsUnknown;
    fProcTable[fLine[Run]];
  end;
end;

function TSynADSP21xxSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynADSP21xxSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynADSP21xxSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynADSP21xxSyn.GetTokenID: TTokenKind;
begin
  Result := fTokenId;
end;

function TSynADSP21xxSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynADSP21xxSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkRegister: Result := fRegisterAttri;
    tkCondition: Result := fConditionAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynADSP21xxSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynADSP21xxSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynADSP21xxSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynADSP21xxSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynADSP21xxSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  {$IFNDEF SYN_CLX}
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_CURRENT_USER;
      // we need some method to make the following statement more universal!
      if OpenKeyReadOnly('\SOFTWARE\Wynand\DSPIDE\1.0') then
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

function TSynADSP21xxSyn.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

    {$IFNDEF SYN_CLX}
    function ReadDspIDESetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;
    begin
      try
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Wynand\DspIDE\1.0\Editor\Highlight',key,false);
      except Result := false; end;
    end;
    {$ENDIF}
var
  tmpNumberAttri    : TSynHighlighterAttributes;
  tmpKeyAttri       : TSynHighlighterAttributes;
  tmpSymbolAttri    : TSynHighlighterAttributes;
  tmpCommentAttri   : TSynHighlighterAttributes;
  tmpConditionAttri : TSynHighlighterAttributes;
  tmpIdentifierAttri: TSynHighlighterAttributes;
  tmpSpaceAttri     : TSynHighlighterAttributes;
  tmpRegisterAttri  : TSynHighlighterAttributes;
  StrLst            : TStringList;

begin  // UseUserSettings
  StrLst := TStringList.Create;
  try
    EnumUserSettings(StrLst);
    if settingIndex >= StrLst.Count then
      Result := false
    else
    begin
      tmpNumberAttri    := TSynHighlighterAttributes.Create('');
      tmpKeyAttri       := TSynHighlighterAttributes.Create('');
      tmpSymbolAttri    := TSynHighlighterAttributes.Create('');
      tmpCommentAttri   := TSynHighlighterAttributes.Create('');
      tmpConditionAttri := TSynHighlighterAttributes.Create('');
      tmpIdentifierAttri:= TSynHighlighterAttributes.Create('');
      tmpSpaceAttri     := TSynHighlighterAttributes.Create('');
      tmpRegisterAttri  := TSynHighlighterAttributes.Create('');

      tmpNumberAttri    .Assign(fNumberAttri);
      tmpKeyAttri       .Assign(fKeyAttri);
      tmpSymbolAttri    .Assign(fSymbolAttri);
      tmpCommentAttri   .Assign(fCommentAttri);
      tmpConditionAttri .Assign(fConditionAttri);
      tmpIdentifierAttri.Assign(fIdentifierAttri);
      tmpSpaceAttri     .Assign(fSpaceAttri);
      tmpRegisterAttri  .Assign(fRegisterAttri);
      {$IFNDEF SYN_CLX}
      Result := ReadDspIDESetting(StrLst[settingIndex],fCommentAttri,'Comment')       and
                ReadDspIDESetting(StrLst[settingIndex],fIdentifierAttri,'Identifier') and
                ReadDspIDESetting(StrLst[settingIndex],fKeyAttri,'Reserved word')     and
                ReadDspIDESetting(StrLst[settingIndex],fNumberAttri,'BinaryNumber')   and
                ReadDspIDESetting(StrLst[settingIndex],fSpaceAttri,'Whitespace')      and
                ReadDspIDESetting(StrLst[settingIndex],fSymbolAttri,'Symbol')         and
                ReadDspIDESetting(StrLst[settingIndex],fConditionAttri,'Condition')   and
                ReadDspIDESetting(StrLst[settingIndex],fRegisterAttri,'Symbol');
      {$ELSE}
      Result := False;
      {$ENDIF}
      if not Result then
      begin
        fNumberAttri     .Assign(tmpNumberAttri);
        fKeyAttri        .Assign(tmpKeyAttri);
        fSymbolAttri     .Assign(tmpSymbolAttri);
        fCommentAttri    .Assign(tmpCommentAttri);
        fConditionAttri  .Assign(tmpConditionAttri);
        fIdentifierAttri .Assign(tmpIdentifierAttri);
        fSpaceAttri      .Assign(tmpSpaceAttri);
        fConditionAttri  .Assign(tmpConditionAttri);
        fRegisterAttri   .Assign(tmpRegisterAttri);
      end;
      tmpNumberAttri    .Free;
      tmpKeyAttri       .Free;
      tmpSymbolAttri    .Free;
      tmpCommentAttri   .Free;
      tmpConditionAttri .Free;
      tmpIdentifierAttri.Free;
      tmpSpaceAttri     .Free;
      tmpRegisterAttri  .Free;
    end;
  finally StrLst.Free; end;
end;

function TSynADSP21xxSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynADSP21xxSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterADSP21xx;
end;

class function TSynADSP21xxSyn.GetLanguageName: string;
begin
  Result := SYNS_LangADSP21xx;
end;

class function TSynADSP21xxSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynADSP21xxSyn);
{$ENDIF}
end.
