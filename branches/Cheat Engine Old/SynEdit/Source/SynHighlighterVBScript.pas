{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVBScript.pas, released 2000-04-18.
The Original Code is based on the lbVBSSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Luiz C. Vaz de Brito.
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

$Id: SynHighlighterVBScript.pas,v 1.15.2.2 2007/05/24 11:17:58 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a VBScript highlighter for SynEdit)
@author(Luiz C. Vaz de Brito, converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(20 January 1999, converted to SynEdit April 18, 2000)
@lastmod(2000-06-23)
The SynHighlighterVBScript unit provides SynEdit with a VisualBasic Script (.vbs) highlighter.
Thanks to Primoz Gabrijelcic and Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERVBSCRIPT}
unit SynHighlighterVBScript;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
{$ELSE}
  Graphics,
  Registry,
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
  TSynVBScriptSyn = class(TSynCustomHighLighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIdentFuncTable: array[0..133] of TIdentFuncTableFunc;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
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
    function Func66: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func133: TtkTokenKind;
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
    procedure SymbolProc;
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
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
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
    Case I in['_', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynVBScriptSyn.InitIdent;
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
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
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
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[133] := Func133;
end;

function TSynVBScriptSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynVBScriptSyn.KeyComp(const aKey: String): Boolean;
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
end; { KeyComp }

function TSynVBScriptSyn.Func15: TtkTokenKind;
begin
  if KeyComp('If') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func17: TtkTokenKind;
begin
  if KeyComp('Each') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func18: TtkTokenKind;
begin
  if KeyComp('Me') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func19: TtkTokenKind;
begin
  if KeyComp('Do') then Result := tkKey else
    if KeyComp('And') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func20: TtkTokenKind;
begin
  if KeyComp('As') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func23: TtkTokenKind;
begin
  if KeyComp('End') then Result := tkKey else
    if KeyComp('In') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func26: TtkTokenKind;
begin
  if KeyComp('Dim') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func28: TtkTokenKind;
begin
  if KeyComp('Case') then Result := tkKey else
    if KeyComp('Call') then Result := tkKey else
      if KeyComp('Is') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func29: TtkTokenKind;
begin
  if KeyComp('On') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func32: TtkTokenKind;
begin
  if KeyComp('Mod') then Result := tkKey else
    if KeyComp('Get') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func33: TtkTokenKind;
begin
  if KeyComp('Or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func35: TtkTokenKind;
begin
  if KeyComp('To') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func36: TtkTokenKind;
begin
  if KeyComp('Rem') then
  begin
    Result := tkComment;
    fStringLen := 0;
    ApostropheProc;
  end
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func37: TtkTokenKind;
begin
  if KeyComp('Let') then Result := tkKey else
    if KeyComp('Like') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func38: TtkTokenKind;
begin
  if KeyComp('Imp') then Result := tkKey else
    if KeyComp('EndIf') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func39: TtkTokenKind;
begin
  if KeyComp('For') then Result := tkKey else
    if KeyComp('Debug') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func41: TtkTokenKind;
begin
  if KeyComp('Else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func42: TtkTokenKind;
begin
  if KeyComp('Sub') then Result := tkKey else
    if KeyComp('New') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func43: TtkTokenKind;
begin
  if KeyComp('False') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func44: TtkTokenKind;
begin
  if KeyComp('Eqv') then Result := tkKey else
    if KeyComp('Set') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func46: TtkTokenKind;
begin
  if KeyComp('Wend') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func47: TtkTokenKind;
begin
  if KeyComp('Then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func48: TtkTokenKind;
begin
  if KeyComp('Erase') then Result := tkKey else
    if KeyComp('Long') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func49: TtkTokenKind;
begin
  if KeyComp('ReDim') then Result := tkKey else
    if KeyComp('Not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func52: TtkTokenKind;
begin
  if KeyComp('Byte') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func53: TtkTokenKind;
begin
  if KeyComp('Enum') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func54: TtkTokenKind;
begin
  if KeyComp('Class') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func55: TtkTokenKind;
begin
  if KeyComp('Shared') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func56: TtkTokenKind;
begin
  if KeyComp('ByRef') then Result := tkKey else
    if KeyComp('ElseIf') then Result := tkKey else
      if KeyComp('LSet') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func57: TtkTokenKind;
begin
  if KeyComp('Xor') then Result := tkKey else
    if KeyComp('While') then Result := tkKey else
      if KeyComp('GoTo') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func58: TtkTokenKind;
begin
  if KeyComp('Loop') then Result := tkKey else
    if KeyComp('Exit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func59: TtkTokenKind;
begin
  if KeyComp('Double') then
    Result := tkKey
  else if KeyComp('Null') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func60: TtkTokenKind;
begin
  if KeyComp('With') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func62: TtkTokenKind;
begin
  if KeyComp('ByVal') then Result := tkKey else
    if KeyComp('RSet') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func63: TtkTokenKind;
begin
  if KeyComp('Next') then Result := tkKey else
    if KeyComp('Public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func64: TtkTokenKind;
begin
  if KeyComp('Select') then Result := tkKey else
    if KeyComp('Boolean') then Result := tkKey else
      if KeyComp('True') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func66: TtkTokenKind;
begin
  if KeyComp('Event') then
     Result := tkKey
  else if KeyComp('Single') then
     Result := tkKey
  else if KeyComp('Type') then
     Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func69: TtkTokenKind;
begin
  if KeyComp('default') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func70: TtkTokenKind;
begin
  if KeyComp('Stop') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func71: TtkTokenKind;
begin
  if KeyComp('Const') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func72: TtkTokenKind;
begin
  if KeyComp('Static') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func74: TtkTokenKind;
begin
  if KeyComp('Error') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func76: TtkTokenKind;
begin
  if KeyComp('Until') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func78: TtkTokenKind;
begin
  if KeyComp('Integer') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func79: TtkTokenKind;
begin
  if KeyComp('Empty') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func81: TtkTokenKind;
begin
  if KeyComp('Resume') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func85: TtkTokenKind;
begin
  if KeyComp('Variant') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func87: TtkTokenKind;
begin
  if KeyComp('Nothing') then
    Result := tkKey
  else if KeyComp('TypeOf') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func89: TtkTokenKind;
begin
  if KeyComp('Option') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func91: TtkTokenKind;
begin
  if KeyComp('Private') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func98: TtkTokenKind;
begin
  if KeyComp('Explicit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func102: TtkTokenKind;
begin
  if KeyComp('Function') then Result := tkKey else
    if KeyComp('Optional') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func105: TtkTokenKind;
begin
  if KeyComp('Randomize') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func107: TtkTokenKind;
begin
  if KeyComp('Currency') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func108: TtkTokenKind;
begin
  if KeyComp('Preserve') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func112: TtkTokenKind;
begin
  if KeyComp('ParamArray') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func118: TtkTokenKind;
begin
  if KeyComp('RaiseEvent') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func126: TtkTokenKind;
begin
  if KeyComp('Implements') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVBScriptSyn.Func133: TtkTokenKind;
begin
  if KeyComp('property') then Result := tkKey else Result := tkIdentifier;
end;

function TSynVBScriptSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynVBScriptSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 134 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynVBScriptSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #39: fProcTable[I] := ApostropheProc;
      #13: fProcTable[I] := CRProc;
      '#': fProcTable[I] := DateProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      #34: fProcTable[I] := StringProc;
      '&', '{', '}', ':', ',', '=', '^', '-',
      '+', '.', '(', ')', ';', '/', '*': fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynVBScriptSyn.Create(AOwner: TComponent);
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
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterVBScript;
  InitIdent;
  MakeMethodTables;
end;

procedure TSynVBScriptSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynVBScriptSyn.ApostropheProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynVBScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynVBScriptSyn.DateProc;
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

procedure TSynVBScriptSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynVBScriptSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynVBScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVBScriptSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynVBScriptSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynVBScriptSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do inc(Run);
end;

procedure TSynVBScriptSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynVBScriptSyn.StringProc;
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

procedure TSynVBScriptSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVBScriptSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TSynVBScriptSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynVBScriptSyn.GetDefaultAttribute(Index: integer):
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

function TSynVBScriptSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynVBScriptSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynVBScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVBScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
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

function TSynVBScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVBScriptSyn.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

function TSynVBScriptSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynVBScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVBScript;
end;

class function TSynVBScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangVBSScript;
end;

function TSynVBScriptSyn.GetSampleSource: string;
begin
  Result := ''' Syntax highlighting'#13#10 +
            'function printNumber()'#13#10 +
            '  number = 12345'#13#10 +
            '  document.write("The number is " + number)'#13#10 +
            '  for i = 0 to 10'#13#10 +
            '    x = x + 1.0'#13#10 +
            '  next'#13#10 +
            'end function';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynVBScriptSyn);
{$ENDIF}
end.
