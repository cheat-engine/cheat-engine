{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterBaan.pas, released 2000-04-21.
The Original Code is based on the mwBaanSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is "riceball".
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

$Id: SynHighlighterBaan.pas,v 1.14 2005/01/28 16:53:20 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Baan syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-04-21)
The SynHighlighterBaan unit provides SynEdit with a Baan syntax highlighter.
Thanks to Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERBAAN}
unit SynHighlighterBaan;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils, Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynBaanSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..183] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func96: TtkTokenKind;
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
    function Func114: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func183: TtkTokenKind;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure ErectProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
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
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
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
    Case I of
      '.', '$', '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynBaanSyn.InitIdent;
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
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[96] := Func96;
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
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[145] := Func145;
  fIdentFuncTable[153] := Func153;
  fIdentFuncTable[154] := Func154;
  fIdentFuncTable[160] := Func160;
  fIdentFuncTable[183] := Func183;
end;

function TSynBaanSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['.', '$', '_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynBaanSyn.KeyComp(const aKey: String): Boolean;
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

function TSynBaanSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func19: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func27: TtkTokenKind;
begin
  if KeyComp('cdecl') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func28: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func30: TtkTokenKind;
begin
  if KeyComp('char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func35: TtkTokenKind;
begin
  if KeyComp('catch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func37: TtkTokenKind;
begin
  if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func38: TtkTokenKind;
begin
  if KeyComp('endif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func39: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func40: TtkTokenKind;
begin
  if KeyComp('table') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func41: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func42: TtkTokenKind;
begin
  if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func43: TtkTokenKind;
begin
  if KeyComp('false') then Result := tkKey else
    if KeyComp('int') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func44: TtkTokenKind;
begin
  if KeyComp('bool') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func47: TtkTokenKind;
begin
  if KeyComp('defined') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func48: TtkTokenKind;
begin
  if KeyComp('long') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func50: TtkTokenKind;
begin
  if KeyComp('void') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func51: TtkTokenKind;
begin
  if KeyComp('delete') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func52: TtkTokenKind;
begin
  if KeyComp('pascal') then Result := tkKey else
    if KeyComp('from') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func53: TtkTokenKind;
begin
  if KeyComp('enum') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func54: TtkTokenKind;
begin
  if KeyComp('class') then Result := tkKey else
    if KeyComp('float') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func56: TtkTokenKind;
begin
  if KeyComp('domain') then Result := tkKey else
    if KeyComp('friend') then Result := tkKey else
      if KeyComp('this') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func57: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else
    if KeyComp('auto') then Result := tkKey else
      if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func58: TtkTokenKind;
begin
  if KeyComp('_cdecl') then Result := tkKey else
    if KeyComp('signed') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func59: TtkTokenKind;
begin
  if KeyComp('double') then Result := tkKey else
    if KeyComp('where') then Result := tkKey else
      if KeyComp('null') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func63: TtkTokenKind;
begin
  if KeyComp('try') then Result := tkKey else
    if KeyComp('public') then Result := tkKey else
      if KeyComp('inline') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func64: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else
    if KeyComp('select') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func69: TtkTokenKind;
begin
  if KeyComp('default') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func70: TtkTokenKind;
begin
  if KeyComp('using') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func71: TtkTokenKind;
begin
  if KeyComp('stdcall') then Result := tkKey else
    if KeyComp('const') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func72: TtkTokenKind;
begin
  if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func73: TtkTokenKind;
begin
  if KeyComp('union') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func74: TtkTokenKind;
begin
  if KeyComp('mutable') then Result := tkKey else
    if KeyComp('fastcall') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func77: TtkTokenKind;
begin
  if KeyComp('namespace') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func78: TtkTokenKind;
begin
  if KeyComp('date.num') then Result := tkVariable else Result := tkIdentifier;
end;

function TSynBaanSyn.Func79: TtkTokenKind;
begin
  if KeyComp('finally') then Result := tkKey else
    if KeyComp('typeid') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func80: TtkTokenKind;
begin
  if KeyComp('sizeof') then Result := tkKey else
    if KeyComp('short') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func81: TtkTokenKind;
begin
  if KeyComp('typedef') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func82: TtkTokenKind;
begin
  if KeyComp('strip$') then Result := tkKey else
    if KeyComp('switch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func83: TtkTokenKind;
begin
  if KeyComp('selectdo') then Result := tkKey else
    if KeyComp('_pascal') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func84: TtkTokenKind;
begin
  if KeyComp('throw') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func86: TtkTokenKind;
begin
  if KeyComp('brp.open') then Result := tkVariable else
    if KeyComp('extern') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func87: TtkTokenKind;
begin
  if KeyComp('__far') then Result := tkKey else
    if KeyComp('endselect') then Result := tkKey else
      if KeyComp('string') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func89: TtkTokenKind;
begin
  if KeyComp('__cdecl') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func91: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else
    if KeyComp('import') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func92: TtkTokenKind;
begin
  if KeyComp('template') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func93: TtkTokenKind;
begin
  if KeyComp('__based') then Result := tkKey else
    if KeyComp('unsigned') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func96: TtkTokenKind;
begin
  if KeyComp('volatile') then Result := tkKey else
    if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func98: TtkTokenKind;
begin
  if KeyComp('export') then Result := tkKey else
    if KeyComp('explicit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func99: TtkTokenKind;
begin
  if KeyComp('typename') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func100: TtkTokenKind;
begin
  if KeyComp('__near') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func101: TtkTokenKind;
begin
  if KeyComp('register') then Result := tkKey else
    if KeyComp('struct') then Result := tkKey else
      if KeyComp('continue') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func102: TtkTokenKind;
begin
  if KeyComp('_stdcall') then Result := tkKey else
    if KeyComp('function') then Result := tkKey else
      if KeyComp('sql.close') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func103: TtkTokenKind;
begin
  if KeyComp('virtual') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func104: TtkTokenKind;
begin
  if KeyComp('__self') then Result := tkKey else
    if KeyComp('wchar_t') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func105: TtkTokenKind;
begin
  if KeyComp('__int32') then Result := tkKey else
    if KeyComp('__int16') then Result := tkKey else
      if KeyComp('__int8') then Result := tkKey else
        if KeyComp('__int64') then Result := tkKey else
          if KeyComp('_fastcall') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func106: TtkTokenKind;
begin
  if KeyComp('protected') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func107: TtkTokenKind;
begin
  if KeyComp('static_ca') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func108: TtkTokenKind;
begin
  if KeyComp('operator') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func114: TtkTokenKind;
begin
  if KeyComp('__pascal') then Result := tkKey else
    if KeyComp('__pascal') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func117: TtkTokenKind;
begin
  if KeyComp('__loadds') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func118: TtkTokenKind;
begin
  if KeyComp('__thread') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func122: TtkTokenKind;
begin
  if KeyComp('_import') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func123: TtkTokenKind;
begin
  if KeyComp('reinterpr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func124: TtkTokenKind;
begin
  if KeyComp('__fastcal') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func125: TtkTokenKind;
begin
  if KeyComp('__try') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func126: TtkTokenKind;
begin
  if KeyComp('__segname') then Result := tkKey else
    if KeyComp('__declspe') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func129: TtkTokenKind;
begin
  if KeyComp('__rtti') then Result := tkKey else
    if KeyComp('_export') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func133: TtkTokenKind;
begin
  if KeyComp('__stdcall') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func135: TtkTokenKind;
begin
  if KeyComp('__except') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func141: TtkTokenKind;
begin
  if KeyComp('interrupt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func145: TtkTokenKind;
begin
  if KeyComp('__segment') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func153: TtkTokenKind;
begin
  if KeyComp('__import') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func154: TtkTokenKind;
begin
  if KeyComp('__fortran') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func160: TtkTokenKind;
begin
  if KeyComp('__export') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.Func183: TtkTokenKind;
begin
  if KeyComp('__interrup') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBaanSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynBaanSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 184 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynBaanSyn.MakeMethodTables;
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
      '|': fProcTable[I] := ErectProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_', '.', '$': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '%': fProcTable[I] := ModSymbolProc;
      '!': fProcTable[I] := NotSymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '+': fProcTable[I] := PlusProc;
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

constructor TSynBaanSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective);
  AddAttribute(fDirectiveAttri);
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
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterBaan;
end;

procedure TSynBaanSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynBaanSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '&':                               {logical and}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynBaanSyn.AtSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynBaanSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynBaanSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynBaanSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynBaanSyn.ColonProc;
begin
  Case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {colon}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynBaanSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #0;
end;

procedure TSynBaanSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.ErectProc;
begin
  inc(Run, 1);                        {Bann Comments}
  fTokenID := tkComment;
  while FLine[Run] <> #0 do
  begin
    case FLine[Run] of
      #10, #13: break;
    end; //case
    inc(Run);
  end; //while
end;

procedure TSynBaanSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
          inc(Run, 3)
        else                           {shift right}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynBaanSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynBaanSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
          inc(Run, 3)
        else                           {shift left}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.ModSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {mod}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {not}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynBaanSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
      ['0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynBaanSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynBaanSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynBaanSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynBaanSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '=':                               {division assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynBaanSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynBaanSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynBaanSyn.StarProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {star}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynBaanSyn.TildeProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynBaanSyn.XOrSymbolProc;
begin
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynBaanSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynBaanSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynBaanSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynBaanSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynBaanSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynBaanSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynBaanSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirectiveAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynBaanSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynBaanSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynBaanSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['.', '$', '_', '0'..'9', 'a'..'z', 'A'..'Z'] + TSynSpecialChars;
end;

function TSynBaanSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterBaan;
end;

class function TSynBaanSyn.GetLanguageName: string;
begin
  Result := SYNS_LangBaan;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynBaanSyn);
{$ENDIF}
end.
