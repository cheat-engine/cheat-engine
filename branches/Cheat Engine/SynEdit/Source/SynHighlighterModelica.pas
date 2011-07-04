{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterModelica.pas, released 2000-11-09.
The Initial Author of this file is Falko Jens Wagner.
Portions created by Falko Jens Wagner are Copyright 2000 Falko Jens Wagner.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterModelica.pas,v 1.13 2005/01/28 16:53:24 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERMODELICA}
unit SynHighlighterModelica;
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
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsString39, rsString34, rsComment);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynModelicaSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: integer;
    fToIdent: PChar;
    fTokenID: TtkTokenKind;
    fTokenPos: integer;
    fIdentFuncTable: array[0..137] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): integer;
    function KeyComp(const aKey: string): boolean;
    function Func17: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func137: TtkTokenKind;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure ColonProc;
    procedure DirectiveProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolProcWithEqual;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure String34Proc;
    procedure String39Proc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber: integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
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
  mHashTable: array[#0..#255] of integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then mHashTable[I] := Ord(I) - 64 else
            if (I > #96) then mHashTable[I] := Ord(I) - 95;
        end;
      else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynModelicaSyn.InitIdent;
var
  I: integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[137] := Func137;
end;

function TSynModelicaSyn.KeyHash(ToHash: PChar): integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynModelicaSyn.KeyComp(const aKey: string): boolean;
var
  I: integer;
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

function TSynModelicaSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func22: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func25: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func26: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func30: TtkTokenKind;
begin
  if KeyComp('der') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func35: TtkTokenKind;
begin
  if KeyComp('or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func39: TtkTokenKind;
begin
  if KeyComp('Real') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func47: TtkTokenKind;
begin
  if KeyComp('final') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func48: TtkTokenKind;
begin
  if KeyComp('block') then Result := tkKey else
    if KeyComp('false') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func51: TtkTokenKind;
begin
  if KeyComp('package') then Result := tkKey else
    if KeyComp('then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func52: TtkTokenKind;
begin
  if KeyComp('not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func54: TtkTokenKind;
begin
  if KeyComp('when') then Result := tkKey else
    if KeyComp('model') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func59: TtkTokenKind;
begin
  if KeyComp('class') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func60: TtkTokenKind;
begin
  if KeyComp('flow') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func62: TtkTokenKind;
begin
  if KeyComp('elseif') then Result := tkKey else
    if KeyComp('while') then Result := tkKey else
      if KeyComp('loop') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func68: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else
    if KeyComp('record') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func70: TtkTokenKind;
begin
  if KeyComp('Boolean') then Result := tkKey else
    if KeyComp('type') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func80: TtkTokenKind;
begin
  if KeyComp('redeclare') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func81: TtkTokenKind;
begin
  if KeyComp('connect') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func84: TtkTokenKind;
begin
  if KeyComp('partial') then Result := tkKey else
    if KeyComp('Integer') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func85: TtkTokenKind;
begin
  if KeyComp('input') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func88: TtkTokenKind;
begin
  if KeyComp('assert') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func91: TtkTokenKind;
begin
  if KeyComp('replaceable') then Result := tkKey else
    if KeyComp('discrete') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func98: TtkTokenKind;
begin
  if KeyComp('extends') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func106: TtkTokenKind;
begin
  if KeyComp('parameter') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func107: TtkTokenKind;
begin
  if KeyComp('external') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func110: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey else
    if KeyComp('equation') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func112: TtkTokenKind;
begin
  if KeyComp('algorithm') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func114: TtkTokenKind;
begin
  if KeyComp('constant') then Result := tkKey else
    if KeyComp('terminate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func115: TtkTokenKind;
begin
  if KeyComp('protected') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func116: TtkTokenKind;
begin
  if KeyComp('connector') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func119: TtkTokenKind;
begin
  if KeyComp('output') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func133: TtkTokenKind;
begin
  if KeyComp('annotation') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.Func137: TtkTokenKind;
begin
  if KeyComp('nondiscrete') then Result := tkKey else Result := tkIdentifier;
end;

function TSynModelicaSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynModelicaSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 138 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynModelicaSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := AsciiCharProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := ColonProc;
      '#': fProcTable[I] := DirectiveProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9':
        fProcTable[I] := NumberProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '.': fProcTable[I] := PointProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      #34: fProcTable[I] := StringProc;
      '~', '[', ']', '@', '{', '}', '(', ')', ';', ',':
        fProcTable[I] := SymbolProc;
      '*', '^', '=', '%', '!':
        fProcTable[I] := SymbolProcWithEqual;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynModelicaSyn.Create(AOwner: TComponent);
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
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterModelica;
  fRange := rsUnknown;
end;

procedure TSynModelicaSyn.SetLine(NewValue: string; LineNumber: integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynModelicaSyn.AndSymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '&'] then
    Inc(Run);
end;

procedure TSynModelicaSyn.AsciiCharProc;
begin
  fRange := rsString39;
  fTokenID := tkString;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13, #39];
  if fLine[Run] = #39 then begin
    fRange := rsUnknown;
    Inc(Run);
  end;
end;

procedure TSynModelicaSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynModelicaSyn.ColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = ':' then
    Inc(Run);
end;

procedure TSynModelicaSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynModelicaSyn.GreaterProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  case fLine[Run] of
    '=': Inc(Run);
    '>': begin
           Inc(Run);
           if fLine[Run] = '=' then
             Inc(Run);
         end;
  end;
end;

procedure TSynModelicaSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynModelicaSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynModelicaSyn.LowerProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then
             Inc(Run);
         end;
  end;
end;

procedure TSynModelicaSyn.MinusProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '-', '>'] then
    Inc(Run);
end;

procedure TSynModelicaSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynModelicaSyn.NumberProc;
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

procedure TSynModelicaSyn.OrSymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynModelicaSyn.PlusProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] in ['=', '+'] then
    Inc(Run);
end;

procedure TSynModelicaSyn.PointProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then
    Inc(Run, 2);
end;

procedure TSynModelicaSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/':
      begin
        fTokenID := tkComment;
        repeat
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
    '*':
      begin
        fRange := rsComment;
        inc(Run);
        if fLine[Run] in [#0, #10, #13] then
          fTokenID := tkComment
        else
          AnsiCProc;
      end;
  else
    fTokenID := tkSymbol;
    if fLine[Run] = '=' then
      Inc(Run);
  end;
end;

procedure TSynModelicaSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynModelicaSyn.StringProc;
begin
  fRange := rsString34;
  Inc(Run);
  if fLine[Run] in [#0, #10, #13] then
    fTokenID := tkString
  else
    String34Proc;
end;

procedure TSynModelicaSyn.SymbolProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynModelicaSyn.SymbolProcWithEqual;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynModelicaSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynModelicaSyn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        inc(Run, 2);
        fRange := rsUnknown;
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynModelicaSyn.String39Proc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkString;
    repeat
      if fLine[Run] = #39 then begin
        inc(Run);
        fRange := rsUnknown;
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynModelicaSyn.String34Proc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkString;
    repeat
      case fLine[Run] of
        #34:
          begin
            Inc(Run);
            fRange := rsUnknown;
            break;
          end;
        #92:
          begin
            Inc(Run);
            if fLine[Run] = #34 then
              Inc(Run);
          end;
      else
        Inc(Run);
      end;
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynModelicaSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: AnsiCProc;
    rsString39: String39Proc;
    rsString34: String34Proc;
  else
    fRange := rsUnknown;
    fProcTable[fLine[Run]];
  end;
end;

function TSynModelicaSyn.GetDefaultAttribute(
  Index: integer): TSynHighlighterAttributes;
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

function TSynModelicaSyn.GetEol: boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynModelicaSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynModelicaSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynModelicaSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynModelicaSyn.GetTokenAttribute: TSynHighlighterAttributes;
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
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynModelicaSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynModelicaSyn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

procedure TSynModelicaSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynModelicaSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynModelicaSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynModelicaSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterModelica;
end;

class function TSynModelicaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangModelica;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynModelicaSyn);
{$ENDIF}
end.
