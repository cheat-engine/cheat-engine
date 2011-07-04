{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVB.pas, released 2000-04-20.
The Original Code is based on the wbADSP21xxSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Max Horvﬂth.
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

$Id: SynHighlighterVB.pas,v 1.15 2005/01/28 16:53:25 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Visual Basic highlighter for SynEdit)
@author(Max Horvﬂth <TheProfessor@gmx.de>, converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(5 December 1999, converted to SynEdit April 21, 2000)
@lastmod(2000-06-23)
The SynHighlighterVB unit provides SynEdit with a Visual Basic (.bas) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERVB}
unit SynHighlighterVB;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
{$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  SynEditHighlighter,
  SynEditTypes,
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
  TSynVBSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..133] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func10: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
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
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
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
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func133: TtkTokenKind;
    procedure SymbolProc;
    procedure ApostropheProc;
    procedure CRProc;
    procedure DateProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
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

procedure TSynVBSyn.InitIdent;
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
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[24] := Func24;
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
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
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
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[133] := Func133;
end;

function TSynVBSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynVBSyn.KeyComp(const aKey: String): Boolean;
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

function TSynVBSyn.Func10: TtkTokenKind;
begin
  if KeyComp('ddb') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func17: TtkTokenKind;
begin
  if KeyComp('each') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func18: TtkTokenKind;
begin
  if KeyComp('me') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func19: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else
    if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func20: TtkTokenKind;
begin
  if KeyComp('as') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func21: TtkTokenKind;
begin
  if KeyComp('cdbl') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func22: TtkTokenKind;
begin
  if KeyComp('abs') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func23: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else
    if KeyComp('end with') then Result := tkKey else
      if KeyComp('asc') then Result := tkKey else
        if KeyComp('tab') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func24: TtkTokenKind;
begin
  if KeyComp('iif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func26: TtkTokenKind;
begin
  if KeyComp('mid') then Result := tkKey else
    if KeyComp('eof') then Result := tkKey else
      if KeyComp('dim') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func27: TtkTokenKind;
begin
  if KeyComp('rgb') then Result := tkKey else
    if KeyComp('base') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func28: TtkTokenKind;
begin
  if KeyComp('fv') then Result := tkKey else
    if KeyComp('beep') then Result := tkKey else
      if KeyComp('call') then Result := tkKey else
        if KeyComp('case') then Result := tkKey else
          if KeyComp('is') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func29: TtkTokenKind;
begin
  if KeyComp('on') then Result := tkKey else
    if KeyComp('chr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func30: TtkTokenKind;
begin
  if KeyComp('loc') then Result := tkKey else
    if KeyComp('date') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func31: TtkTokenKind;
begin
  if KeyComp('dir') then Result := tkKey else
    if KeyComp('len') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func32: TtkTokenKind;
begin
  if KeyComp('mod') then Result := tkKey else
    if KeyComp('get') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func33: TtkTokenKind;
begin
  if KeyComp('name') then Result := tkKey else
    if KeyComp('lof') then Result := tkKey else
      if KeyComp('or') then Result := tkKey else
        if KeyComp('cdate') then Result := tkKey else
          if KeyComp('cdate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func34: TtkTokenKind;
begin
  if KeyComp('log') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func35: TtkTokenKind;
begin
  if KeyComp('val') then Result := tkKey else
    if KeyComp('tan') then Result := tkKey else
      if KeyComp('to') then Result := tkKey else
        if KeyComp('atn') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func36: TtkTokenKind;
begin
  if KeyComp('clng') then Result := tkKey else
    if KeyComp('rnd') then Result := tkKey else
      if KeyComp('rem') then
      begin
        ApostropheProc;
        fStringLen:=0;
        Result := tkComment;
      end
      else
        Result := tkIdentifier;
end;

function TSynVBSyn.Func37: TtkTokenKind;
begin
  if KeyComp('cos') then Result := tkKey else
    if KeyComp('begin') then Result := tkKey else
      if KeyComp('let') then Result := tkKey else
        if KeyComp('hex') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func38: TtkTokenKind;
begin
  if KeyComp('spc') then Result := tkKey else
    if KeyComp('pv') then Result := tkKey else
      if KeyComp('imp') then Result := tkKey else
        if KeyComp('oct') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func39: TtkTokenKind;
begin
  if KeyComp('dateadd') then Result := tkKey else
    if KeyComp('clear') then Result := tkKey else
      if KeyComp('for') then Result := tkKey else
        if KeyComp('fix') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func40: TtkTokenKind;
begin
  if KeyComp('seek') then Result := tkKey else
    if KeyComp('sgn') then Result := tkKey else
      if KeyComp('line') then Result := tkKey else
        if KeyComp('lcase') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func41: TtkTokenKind;
begin
  if KeyComp('lock') then Result := tkKey else
    if KeyComp('err') then Result := tkKey else
      if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func42: TtkTokenKind;
begin
  if KeyComp('chdir') then Result := tkKey else
    if KeyComp('sub') then Result := tkKey else
      if KeyComp('sin') then Result := tkKey else
        if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func43: TtkTokenKind;
begin
  if KeyComp('csng') then Result := tkKey else
    if KeyComp('int') then Result := tkKey else
      if KeyComp('left') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func44: TtkTokenKind;
begin
  if KeyComp('cvar') then Result := tkKey else
    if KeyComp('kill') then Result := tkKey else
      if KeyComp('set') then Result := tkKey else
        if KeyComp('rate') then Result := tkKey else
          if KeyComp('space') then Result := tkKey else
            if KeyComp('eqv') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func45: TtkTokenKind;
begin
  if KeyComp('ccur') then Result := tkKey else
    if KeyComp('irr') then Result := tkKey else
      if KeyComp('sln') then Result := tkKey else
        if KeyComp('exp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func46: TtkTokenKind;
begin
  if KeyComp('cint') then Result := tkKey else
    if KeyComp('wend') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func47: TtkTokenKind;
begin
  if KeyComp('time') then Result := tkKey else
    if KeyComp('cbool') then Result := tkKey else
      if KeyComp('then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func48: TtkTokenKind;
begin
  if KeyComp('long') then Result := tkKey else
    if KeyComp('syd') then Result := tkKey else
      if KeyComp('erase') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func49: TtkTokenKind;
begin
  if KeyComp('ucase') then Result := tkKey else
    if KeyComp('redim') then Result := tkKey else
      if KeyComp('pmt') then Result := tkKey else
        if KeyComp('not') then Result := tkKey else
          if KeyComp('not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func50: TtkTokenKind;
begin
  if KeyComp('circle') then Result := tkKey else
    if KeyComp('open') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func52: TtkTokenKind;
begin
  if KeyComp('raise') then Result := tkKey else
    if KeyComp('now') then Result := tkKey else
      if KeyComp('npv') then Result := tkKey else
        if KeyComp('byte') then Result := tkKey else
          if KeyComp('form') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func53: TtkTokenKind;
begin
  if KeyComp('nper') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func54: TtkTokenKind;
begin
  if KeyComp('sqr') then Result := tkKey else
    if KeyComp('class') then Result := tkKey else
      if KeyComp('close') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func55: TtkTokenKind;
begin
  if KeyComp('mkdir') then Result := tkKey else
    if KeyComp('datediff') then Result := tkKey else
      if KeyComp('object') then Result := tkKey else
        if KeyComp('cbyte') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func56: TtkTokenKind;
begin
  if KeyComp('lset') then Result := tkKey else
    if KeyComp('shell') then Result := tkKey else
      if KeyComp('elseif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func57: TtkTokenKind;
begin
  if KeyComp('str') then Result := tkKey else
    if KeyComp('goto') then Result := tkKey else
      if KeyComp('xor') then Result := tkKey else
        if KeyComp('put') then Result := tkKey else
          if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func58: TtkTokenKind;
begin
  if KeyComp('mirr') then Result := tkKey else
    if KeyComp('isdate') then Result := tkKey else
      if KeyComp('ipmt') then Result := tkKey else
        if KeyComp('exit') then Result := tkKey else
          if KeyComp('loop') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func59: TtkTokenKind;
begin
  if KeyComp('double') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func60: TtkTokenKind;
begin
  if KeyComp('with') then Result := tkKey else
    if KeyComp('second') then Result := tkKey else
      if KeyComp('cstr') then Result := tkKey else
        if KeyComp('pset') then Result := tkKey else
          if KeyComp('trim') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func62: TtkTokenKind;
begin
  if KeyComp('rmdir') then Result := tkKey else
    if KeyComp('right') then Result := tkKey else
      if KeyComp('rset') then Result := tkKey else
        if KeyComp('hour') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func63: TtkTokenKind;
begin
  if KeyComp('next') then Result := tkKey else
    if KeyComp('filelen') then Result := tkKey else
      if KeyComp('public') then Result := tkKey else
        if KeyComp('command') then Result := tkKey else
          if KeyComp('array') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func64: TtkTokenKind;
begin
  if KeyComp('select') then Result := tkKey else
    if KeyComp('gosub') then Result := tkKey else
      if KeyComp('boolean') then Result := tkKey else
        if KeyComp('width') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func65: TtkTokenKind;
begin
  if KeyComp('timer') then Result := tkKey else
    if KeyComp('ppmt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func66: TtkTokenKind;
begin
  if KeyComp('freefile') then Result := tkKey else
    if KeyComp('single') then Result := tkKey else
      if KeyComp('cverr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func67: TtkTokenKind;
begin
  if KeyComp('reset') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func68: TtkTokenKind;
begin
  if KeyComp('lbound') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func69: TtkTokenKind;
begin
  if KeyComp('chdrive') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func70: TtkTokenKind;
begin
  if KeyComp('stop') then Result := tkKey else
    if KeyComp('month') then Result := tkKey else
      if KeyComp('module') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func71: TtkTokenKind;
begin
  if KeyComp('const') then Result := tkKey else
    if KeyComp('compare') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func72: TtkTokenKind;
begin
  if KeyComp('ltrim') then Result := tkKey else
    if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func73: TtkTokenKind;
begin
  if KeyComp('curdir') then Result := tkKey else
    if KeyComp('format') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func74: TtkTokenKind;
begin
  if KeyComp('weekday') then Result := tkKey else
    if KeyComp('error') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func75: TtkTokenKind;
begin
  if KeyComp('write') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func76: TtkTokenKind;
begin
  if KeyComp('until') then Result := tkKey else
    if KeyComp('unlock') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func77: TtkTokenKind;
begin
  if KeyComp('ubound') then Result := tkKey else
    if KeyComp('print') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func78: TtkTokenKind;
begin
  if KeyComp('integer') then Result := tkKey else
    if KeyComp('rtrim') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func79: TtkTokenKind;
begin
  if KeyComp('empty') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func80: TtkTokenKind;
begin
  if KeyComp('instr') then Result := tkKey else
    if KeyComp('input') then Result := tkKey else
      if KeyComp('msgbox') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func81: TtkTokenKind;
begin
  if KeyComp('deftype') then Result := tkKey else
    if KeyComp('resume') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func82: TtkTokenKind;
begin
  if KeyComp('minute') then Result := tkKey else
    if KeyComp('switch') then Result := tkKey else
      if KeyComp('qbcolor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func83: TtkTokenKind;
begin
  if KeyComp('isobject') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func85: TtkTokenKind;
begin
  if KeyComp('variant') then Result := tkKey else
    if KeyComp('datepart') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func87: TtkTokenKind;
begin
  if KeyComp('string') then Result := tkKey else
    if KeyComp('nothing') then Result := tkKey else
      if KeyComp('isnull') then Result := tkKey else
        if KeyComp('getobject') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func89: TtkTokenKind;
begin
  if KeyComp('option') then Result := tkKey else
    if KeyComp('option') then Result := tkKey else
      if KeyComp('option') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func91: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else
    if KeyComp('datevalue') then Result := tkKey else
      if KeyComp('fileattr') then Result := tkKey else
        if KeyComp('isarray') then Result := tkKey else
          if KeyComp('fileattr') then Result := tkKey else
            if KeyComp('filecopy') then Result := tkKey else
              if KeyComp('getattr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func94: TtkTokenKind;
begin
  if KeyComp('dateserial') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func96: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func97: TtkTokenKind;
begin
  if KeyComp('environ') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func98: TtkTokenKind;
begin
  if KeyComp('explicit') then Result := tkKey else
    if KeyComp('explicit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func99: TtkTokenKind;
begin
  if KeyComp('typename') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func101: TtkTokenKind;
begin
  if KeyComp('system') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func102: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey else
    if KeyComp('version') then Result := tkKey else
      if KeyComp('sendkeys') then Result := tkKey else
        if KeyComp('iserror') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func103: TtkTokenKind;
begin
  if KeyComp('setattr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func104: TtkTokenKind;
begin
  if KeyComp('doevents') then Result := tkKey else
    if KeyComp('strcomp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func105: TtkTokenKind;
begin
  if KeyComp('randomize') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func107: TtkTokenKind;
begin
  if KeyComp('isempty') then Result := tkKey else
    if KeyComp('currency') then Result := tkKey else
      if KeyComp('vartype') then Result := tkKey else
        if KeyComp('createobject') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func108: TtkTokenKind;
begin
  if KeyComp('timevalue') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func109: TtkTokenKind;
begin
  if KeyComp('filedatetime') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func111: TtkTokenKind;
begin
  if KeyComp('isnumeric') then Result := tkKey else
    if KeyComp('strconv') then Result := tkKey else
      if KeyComp('timeserial') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func114: TtkTokenKind;
begin
  if KeyComp('appactivate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func116: TtkTokenKind;
begin
  if KeyComp('attribute') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func118: TtkTokenKind;
begin
  if KeyComp('ismissing') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.Func133: TtkTokenKind;
begin
  if KeyComp('property') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynVBSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 134 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynVBSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := SymbolProc;
      #39: fProcTable[I] := ApostropheProc;
      '}': fProcTable[I] := SymbolProc;
      '{': fProcTable[I] := SymbolProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := SymbolProc;
      ',': fProcTable[I] := SymbolProc;
      '#': fProcTable[I] := DateProc;
      '=': fProcTable[I] := SymbolProc;
      '^': fProcTable[I] := SymbolProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := SymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '+': fProcTable[I] := SymbolProc;
      '.': fProcTable[I] := SymbolProc;
      ')': fProcTable[I] := SymbolProc;
      '(': fProcTable[I] := SymbolProc;
      ';': fProcTable[I] := SymbolProc;
      '/': fProcTable[I] := SymbolProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      '*': fProcTable[I] := SymbolProc;
      #34: fProcTable[I] := StringProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynVBSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
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
  fDefaultFilter := SYNS_FilterVisualBASIC;
end;

procedure TSynVBSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynVBSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVBSyn.ApostropheProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynVBSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynVBSyn.DateProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = '#';
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynVBSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynVBSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynVBSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVBSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynVBSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynVBSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do inc(Run);
end;

procedure TSynVBSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynVBSyn.StringProc;
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
end;

procedure TSynVBSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynVBSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynVBSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
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

function TSynVBSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynVBSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynVBSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVBSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TSynVBSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVBSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynVBSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynVBSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVisualBASIC;
end;

class function TSynVBSyn.GetLanguageName: string;
begin
  Result := SYNS_LangVisualBASIC;
end;

function TSynVBSyn.GetSampleSource: String;
begin
  Result := ''' Syntax highlighting'#13#10+
            'Function PrintNumber'#13#10+
            '  Dim Number'#13#10+
            '  Dim X'#13#10+
            ''#13#10+
            '  Number = 123456'#13#10+
            '  Response.Write "The number is " & number'#13#10+
            ''#13#10+
            '  For I = 0 To Number'#13#10+
            '    X = X + &h4c'#13#10+
            '    X = X - &o8'#13#10+
            '    X = X + 1.0'#13#10+
            '  Next'#13#10+
            ''#13#10+
            '  I = I + @;  '' illegal character'#13#10+
            'End Function';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynVBSyn);
{$ENDIF}
end.
