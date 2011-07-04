{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterFoxpro.pas, released 2000-04-21.
The Original Code is based on the mwFoxproSyn.pas file from the
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

$Id: SynHighlighterFoxpro.pas,v 1.13 2005/01/28 16:53:22 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Foxpro Syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterFoxpro unit provides SynEdit with a Foxpro syntax highlighter.
Thanks to Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERFOXPRO}
unit SynHighlighterFoxpro;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynFoxproSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..160] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
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
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
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
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func160: TtkTokenKind;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
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

procedure TSynFoxproSyn.InitIdent;
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
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
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
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
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
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[160] := Func160;
end;

function TSynFoxproSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynFoxproSyn.KeyComp(const aKey: String): Boolean;
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

function TSynFoxproSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func19: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else
    if KeyComp('and') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func21: TtkTokenKind;
begin
  if KeyComp('at') then Result := tkKey else
    if KeyComp('bar') then Result := tkKey else
      if KeyComp('of') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func22: TtkTokenKind;
begin
  if KeyComp('go') then Result := tkKey else
    if KeyComp('abs') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func23: TtkTokenKind;
begin
  if KeyComp('bof') then Result := tkKey else
    if KeyComp('asc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func25: TtkTokenKind;
begin
  if KeyComp('all') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func26: TtkTokenKind;
begin
  if KeyComp('dele') then Result := tkKey else
    if KeyComp('eof') then Result := tkKey else
      if KeyComp('dim') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func27: TtkTokenKind;
begin
  if KeyComp('rgb') then Result := tkKey else
    if KeyComp('off') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func28: TtkTokenKind;
begin
  if KeyComp('tag') then Result := tkKey else
    if KeyComp('read') then Result := tkKey else
      if KeyComp('call') then Result := tkKey else
        if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func29: TtkTokenKind;
begin
  if KeyComp('on') then Result := tkKey else
    if KeyComp('blan') then Result := tkKey else
      if KeyComp('chr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func30: TtkTokenKind;
begin
  if KeyComp('date') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func31: TtkTokenKind;
begin
  if KeyComp('echo') then Result := tkKey else
    if KeyComp('bell') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func32: TtkTokenKind;
begin
  if KeyComp('get') then Result := tkKey else
    if KeyComp('again') then Result := tkKey else
      if KeyComp('file') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func33: TtkTokenKind;
begin
  if KeyComp('acti') then Result := tkKey else
    if KeyComp('or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func35: TtkTokenKind;
begin
  if KeyComp('val') then Result := tkKey else
    if KeyComp('to') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func36: TtkTokenKind;
begin
  if KeyComp('para') then Result := tkKey else
    if KeyComp('gath') then Result := tkKey else
      if KeyComp('rela') then Result := tkKey else
        if KeyComp('field') then Result := tkKey else
          if KeyComp('atan') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func37: TtkTokenKind;
begin
  if KeyComp('begin') then Result := tkKey else
    if KeyComp('scan') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func38: TtkTokenKind;
begin
  if KeyComp('appe') then Result := tkKey else
    if KeyComp('endif') then Result := tkKey else
      if KeyComp('cancel') then Result := tkKey else
        if KeyComp('near') then Result := tkKey else
          if KeyComp('edit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func39: TtkTokenKind;
begin
  if KeyComp('fill') then Result := tkKey else
    if KeyComp('for') then Result := tkKey else
      if KeyComp('clear') then Result := tkKey else
        if KeyComp('debug') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func40: TtkTokenKind;
begin
  if KeyComp('table') then Result := tkKey else
    if KeyComp('blank') then Result := tkKey else
      if KeyComp('seek') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func41: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
    if KeyComp('help') then Result := tkKey else
      if KeyComp('modi') then Result := tkKey else
        if KeyComp('box') then Result := tkKey else
          if KeyComp('lock') then Result := tkKey else
            if KeyComp('key') then Result := tkKey else
              if KeyComp('sele') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func42: TtkTokenKind;
begin
  if KeyComp('enddo') then Result := tkKey else
    if KeyComp('alias') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func43: TtkTokenKind;
begin
  if KeyComp('define') then Result := tkKey else
    if KeyComp('scat') then Result := tkKey else
      if KeyComp('local') then Result := tkKey else
        if KeyComp('mark') then Result := tkKey else
          if KeyComp('ansi') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func44: TtkTokenKind;
begin
  if KeyComp('set') then Result := tkKey else
    if KeyComp('clock') then Result := tkKey else
      if KeyComp('func') then Result := tkKey else
        if KeyComp('comm') then Result := tkKey else
          if KeyComp('talk') then Result := tkKey else
            if KeyComp('space') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func45: TtkTokenKind;
begin
  if KeyComp('modal') then Result := tkKey else
    if KeyComp('use') then Result := tkKey else
      if KeyComp('say') then Result := tkKey else
        if KeyComp('path') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func46: TtkTokenKind;
begin
  if KeyComp('memo') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func48: TtkTokenKind;
begin
  if KeyComp('blink') then Result := tkKey else
    if KeyComp('accept') then Result := tkKey else
      if KeyComp('build') then Result := tkKey else
        if KeyComp('device') then Result := tkKey else
          if KeyComp('fixed') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func49: TtkTokenKind;
begin
  if KeyComp('escape') then Result := tkKey else
    if KeyComp('not') then Result := tkKey else
      if KeyComp('year') then Result := tkKey else
        if KeyComp('clos') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func51: TtkTokenKind;
begin
  if KeyComp('top') then Result := tkKey else
    if KeyComp('repl') then Result := tkKey else
      if KeyComp('delete') then Result := tkKey else
        if KeyComp('files') then Result := tkKey else
          if KeyComp('endcase') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func52: TtkTokenKind;
begin
  if KeyComp('create') then Result := tkKey else
    if KeyComp('from') then Result := tkKey else
      if KeyComp('repla') then Result := tkKey else
        if KeyComp('proc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func53: TtkTokenKind;
begin
  if KeyComp('wait') then Result := tkKey else
    if KeyComp('exact') then Result := tkKey else
      if KeyComp('menu') then Result := tkKey else
        if KeyComp('scheme') then Result := tkKey else
          if KeyComp('database') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func54: TtkTokenKind;
begin
  if KeyComp('class') then Result := tkKey else
    if KeyComp('close') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func55: TtkTokenKind;
begin
  if KeyComp('recno') then Result := tkKey else
    if KeyComp('deleted') then Result := tkKey else
      if KeyComp('skip') then Result := tkKey else
        if KeyComp('fields') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func56: TtkTokenKind;
begin
  if KeyComp('index') then Result := tkKey else
    if KeyComp('append') then Result := tkKey else
      if KeyComp('fopen') then Result := tkKey else
        if KeyComp('this') then Result := tkKey else
          if KeyComp('locate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func57: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else
    if KeyComp('bott') then Result := tkKey else
      if KeyComp('str') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func58: TtkTokenKind;
begin
  if KeyComp('machelp') then Result := tkKey else
    if KeyComp('mackey') then Result := tkKey else
      if KeyComp('brow') then Result := tkKey else
        if KeyComp('into') then Result := tkKey else
          if KeyComp('fcreate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func59: TtkTokenKind;
begin
  if KeyComp('view') then Result := tkKey else
    if KeyComp('copy') then Result := tkKey else
      if KeyComp('gather') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func60: TtkTokenKind;
begin
  if KeyComp('with') then Result := tkKey else
    if KeyComp('endscan') then Result := tkKey else
      if KeyComp('order') then Result := tkKey else
        if KeyComp('fclose') then Result := tkKey else
          if KeyComp('replace') then Result := tkKey else
            if KeyComp('step') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func61: TtkTokenKind;
begin
  if KeyComp('proced') then Result := tkKey else
    if KeyComp('atline') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func62: TtkTokenKind;
begin
  if KeyComp('endfor') then Result := tkKey else
    if KeyComp('border') then Result := tkKey else
      if KeyComp('margin') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func63: TtkTokenKind;
begin
  if KeyComp('color') then Result := tkKey else
    if KeyComp('array') then Result := tkKey else
      if KeyComp('command') then Result := tkKey else
        if KeyComp('topic') then Result := tkKey else
          if KeyComp('next') then Result := tkKey else
            if KeyComp('scatt') then Result := tkKey else
              if KeyComp('public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func64: TtkTokenKind;
begin
  if KeyComp('retu') then Result := tkKey else
    if KeyComp('select') then Result := tkKey else
      if KeyComp('screen') then Result := tkKey else
        if KeyComp('push') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func65: TtkTokenKind;
begin
  if KeyComp('carry') then Result := tkKey else
    if KeyComp('release') then Result := tkKey else
      if KeyComp('relati') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func66: TtkTokenKind;
begin
  if KeyComp('decimals') then Result := tkKey else
    if KeyComp('event') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func67: TtkTokenKind;
begin
  if KeyComp('quit') then Result := tkKey else
    if KeyComp('headings') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func68: TtkTokenKind;
begin
  if KeyComp('collate') then Result := tkKey else
    if KeyComp('region') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func69: TtkTokenKind;
begin
  if KeyComp('message') then Result := tkKey else
    if KeyComp('default') then Result := tkKey else
      if KeyComp('text') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func70: TtkTokenKind;
begin
  if KeyComp('filter') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func72: TtkTokenKind;
begin
  if KeyComp('modify') then Result := tkKey else
    if KeyComp('schemes') then Result := tkKey else
      if KeyComp('memvar') then Result := tkKey else
        if KeyComp('databases') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func73: TtkTokenKind;
begin
  if KeyComp('functi') then Result := tkKey else
    if KeyComp('curdir') then Result := tkKey else
      if KeyComp('mouse') then Result := tkKey else
        if KeyComp('format') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func74: TtkTokenKind;
begin
  if KeyComp('between') then Result := tkKey else
    if KeyComp('point') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func75: TtkTokenKind;
begin
  if KeyComp('endproc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func76: TtkTokenKind;
begin
  if KeyComp('safety') then Result := tkKey else
    if KeyComp('xcmdfile') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func77: TtkTokenKind;
begin
  if KeyComp('classlib') then Result := tkKey else
    if KeyComp('store') then Result := tkKey else
      if KeyComp('delimite') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func78: TtkTokenKind;
begin
  if KeyComp('confirm') then Result := tkKey else
    if KeyComp('caption') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func79: TtkTokenKind;
begin
  if KeyComp('empty') then Result := tkKey else
    if KeyComp('refresh') then Result := tkKey else
      if KeyComp('fcount') then Result := tkKey else
        if KeyComp('palette') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func80: TtkTokenKind;
begin
  if KeyComp('capslock') then Result := tkKey else
    if KeyComp('backcolor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func81: TtkTokenKind;
begin
  if KeyComp('hours') then Result := tkKey else
    if KeyComp('fwrite') then Result := tkKey else
      if KeyComp('activate') then Result := tkKey else
        if KeyComp('regional') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func82: TtkTokenKind;
begin
  if KeyComp('commands') then Result := tkKey else
    if KeyComp('fputs') then Result := tkKey else
      if KeyComp('nowait') then Result := tkKey else
        if KeyComp('browse') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func83: TtkTokenKind;
begin
  if KeyComp('console') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func85: TtkTokenKind;
begin
  if KeyComp('library') then Result := tkKey else
    if KeyComp('bottom') then Result := tkKey else
      if KeyComp('events') then Result := tkKey else
        if KeyComp('typeahead') then Result := tkKey else
          if KeyComp('insert') then Result := tkKey else
            if KeyComp('alltrim') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func86: TtkTokenKind;
begin
  if KeyComp('display') then Result := tkKey else
    if KeyComp('scatter') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func87: TtkTokenKind;
begin
  if KeyComp('unique') then Result := tkKey else
    if KeyComp('sticky') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func88: TtkTokenKind;
begin
  if KeyComp('aplabout') then Result := tkKey else
    if KeyComp('keycomp') then Result := tkKey else
      if KeyComp('volume') then Result := tkKey else
        if KeyComp('window') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func89: TtkTokenKind;
begin
  if KeyComp('notify') then Result := tkKey else
    if KeyComp('shadows') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func90: TtkTokenKind;
begin
  if KeyComp('readborder') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func91: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func94: TtkTokenKind;
begin
  if KeyComp('barcount') then Result := tkKey else
    if KeyComp('cursor') then Result := tkKey else
      if KeyComp('relation') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func95: TtkTokenKind;
begin
  if KeyComp('odometer') then Result := tkKey else
    if KeyComp('_screen') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func96: TtkTokenKind;
begin
  if KeyComp('compatible') then Result := tkKey else
    if KeyComp('alternate') then Result := tkKey else
      if KeyComp('fullpath') then Result := tkKey else
        if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func97: TtkTokenKind;
begin
  if KeyComp('parameter') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func98: TtkTokenKind;
begin
  if KeyComp('udfparms') then Result := tkKey else
    if KeyComp('prompt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func100: TtkTokenKind;
begin
  if KeyComp('scoreboard') then Result := tkKey else
    if KeyComp('status') then Result := tkKey else
      if KeyComp('printer') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func101: TtkTokenKind;
begin
  if KeyComp('pdsetup') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func102: TtkTokenKind;
begin
  if KeyComp('dimension') then Result := tkKey else
    if KeyComp('function') then Result := tkKey else
      if KeyComp('blocksize') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func104: TtkTokenKind;
begin
  if KeyComp('autosave') then Result := tkKey else
    if KeyComp('resource') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func105: TtkTokenKind;
begin
  if KeyComp('procedure') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func106: TtkTokenKind;
begin
  if KeyComp('century') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func107: TtkTokenKind;
begin
  if KeyComp('macdesktop') then Result := tkKey else
    if KeyComp('currency') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func108: TtkTokenKind;
begin
  if KeyComp('thisform') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func110: TtkTokenKind;
begin
  if KeyComp('memowidth') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func111: TtkTokenKind;
begin
  if KeyComp('helpfilter') then Result := tkKey else
    if KeyComp('ansitooem') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func112: TtkTokenKind;
begin
  if KeyComp('trbetween') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func113: TtkTokenKind;
begin
  if KeyComp('separator') then Result := tkKey else
    if KeyComp('optimize') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func114: TtkTokenKind;
begin
  if KeyComp('delimiters') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func116: TtkTokenKind;
begin
  if KeyComp('sysmenu') then Result := tkKey else
    if KeyComp('parameters') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func117: TtkTokenKind;
begin
  if KeyComp('textmerge') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func118: TtkTokenKind;
begin
  if KeyComp('reprocess') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func119: TtkTokenKind;
begin
  if KeyComp('barprompt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func120: TtkTokenKind;
begin
  if KeyComp('brstatus') then Result := tkKey else
    if KeyComp('nocptrans') then Result := tkKey else
      if KeyComp('exclusive') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func122: TtkTokenKind;
begin
  if KeyComp('otherwise') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func127: TtkTokenKind;
begin
  if KeyComp('logerrors') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func131: TtkTokenKind;
begin
  if KeyComp('development') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func133: TtkTokenKind;
begin
  if KeyComp('dohistory') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func135: TtkTokenKind;
begin
  if KeyComp('intensity') then Result := tkKey else
    if KeyComp('multilocks') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.Func160: TtkTokenKind;
begin
  if KeyComp('_msysmenu') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFoxproSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynFoxproSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 161 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynFoxproSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := AsciiCharProc;
      '@': fProcTable[I] := AtSymbolProc;
      '{': fProcTable[I] := BraceOpenProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := ColonProc;
      ',': fProcTable[I] := CommaProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
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
      '?': fProcTable[I] := QuestionProc;
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
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynFoxproSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  AddAttribute(fKeyAttri);
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
  fDefaultFilter := SYNS_FilterFoxpro;
end;

procedure TSynFoxproSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynFoxproSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '&':                               {Comments}
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: break;
          end; //case
          inc(Run);
        end;
      end;
  else                                 {and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFoxproSyn.AsciiCharProc;
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

procedure TSynFoxproSyn.AtSymbolProc;
begin
  fTokenID := tkKey;
  inc(Run);
end;

procedure TSynFoxproSyn.BraceOpenProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = '}';
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynFoxproSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynFoxproSyn.ColonProc;
begin
  {colon}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.EqualProc;
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

procedure TSynFoxproSyn.GreaterProc;
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

procedure TSynFoxproSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynFoxproSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynFoxproSyn.LowerProc;
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

procedure TSynFoxproSyn.MinusProc;
begin
  {subtract}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.ModSymbolProc;
begin
  {mod}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.NotSymbolProc;
begin
  {not}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynFoxproSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
      ['0'..'9', '.', 'x', 'X', 'e', 'E', 'f', 'F'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynFoxproSyn.OrSymbolProc;
begin
  {or}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.PlusProc;
begin
  {subtract}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.PointProc;
begin
  if ((UpCase(FLine[Run + 1]) = 'T')           {.t.}
       or (UpCase(FLine[Run + 1]) = 'F'))      {.f.}
     and (FLine[Run + 2] = '.') then
  begin
      inc(Run, 3);
      fTokenID := tkSymbol;
  end
  else if (((UpCase(FLine[Run + 1]) = 'A')
              and (UpCase(FLine[Run + 2]) = 'N')
              and (UpCase(FLine[Run + 3]) = 'D'))    {.and.}
           or ((UpCase(FLine[Run + 1]) = 'N')
              and (UpCase(FLine[Run + 2]) = 'O')
              and (UpCase(FLine[Run + 3]) = 'T')))    {.not.}
          and (FLine[Run + 4] = '.') then
  begin
      inc(Run, 5);
      fTokenID := tkSymbol;
  end
  else if (UpCase(FLine[Run + 1]) = 'O') 
          and (UpCase(FLine[Run + 2]) = 'R')
          and (FLine[Run + 3] = '.') then  {.or.}
  begin
      inc(Run, 4);
      fTokenID := tkSymbol;
  end
  else                                 {point}
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynFoxproSyn.QuestionProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SlashProc;
begin
  {division}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynFoxproSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.StarProc;
begin
  if Run = 0 then
  begin                        {Foxpro Comments}
    inc(Run);
    fTokenID := tkComment;
    while FLine[Run] <> #0 do
    begin
      case FLine[Run] of
        #10, #13: break;
      end;
      inc(Run);
    end;
  end else begin
    {star}
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynFoxproSyn.StringProc;
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

procedure TSynFoxproSyn.TildeProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynFoxproSyn.XOrSymbolProc;
begin
  {xor}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynFoxproSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynFoxproSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynFoxproSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynFoxproSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynFoxproSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynFoxproSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynFoxproSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynFoxproSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynFoxproSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynFoxproSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterFoxpro;
end;

class function TSynFoxproSyn.GetLanguageName: string;                    
begin
  Result := SYNS_LangFoxpro;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynFoxproSyn);
{$ENDIF}
end.
