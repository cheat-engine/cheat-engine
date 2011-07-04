{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterIDL.pas, released 2001-10-15.
Description: CORBA IDL Parser/Highlighter
The initial author of this file is P.L. Polak.
Copyright (c) 2001, all rights reserved.

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

$Id: SynHighlighterIDL.pas,v 1.9 2005/01/28 16:53:23 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERIDL}
unit SynHighlighterIDL;
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

Type
  TtkTokenKind = (
    tkComment,
    tkDatatype,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkPreprocessor,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsString, rsChar);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 152;

type
  TSynIdlSyn = class(TSynCustomHighlighter)
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
    fCommentAttri: TSynHighlighterAttributes;
    fDatatypeAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func25: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func152: TtkTokenKind;
    procedure IdentProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CommentOpenProc;
    procedure CommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure CharOpenProc;
    procedure CharProc;
    procedure PreProcessorProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
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
    property DatatypeAttri: TSynHighlighterAttributes read fDatatypeAttri write fDatatypeAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read fPreprocessorAttri write fPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
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

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            mHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mHashTable[I] := Ord(I) - 95;
        end;
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynIdlSyn.InitIdent;
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
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[152] := Func152;
end;

function TSynIdlSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynIdlSyn.KeyComp(const aKey: String): Boolean;
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

function TSynIdlSyn.Func25: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func32: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func34: TtkTokenKind;
begin
  if KeyComp('char') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func43: TtkTokenKind;
begin
  if KeyComp('FALSE') then Result := tkKey else
    if KeyComp('any') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func48: TtkTokenKind;
begin
  if KeyComp('local') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func52: TtkTokenKind;
begin
  if KeyComp('long') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func53: TtkTokenKind;
begin
  if KeyComp('fixed') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func54: TtkTokenKind;
begin
  if KeyComp('void') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func57: TtkTokenKind;
begin
  if KeyComp('enum') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func58: TtkTokenKind;
begin
  if KeyComp('wchar') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func59: TtkTokenKind;
begin
  if KeyComp('out') then Result := tkKey else
    if KeyComp('float') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func60: TtkTokenKind;
begin
  if KeyComp('Object') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func64: TtkTokenKind;
begin
  if KeyComp('TRUE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func65: TtkTokenKind;
begin
  if KeyComp('double') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func68: TtkTokenKind;
begin
  if KeyComp('octet') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func71: TtkTokenKind;
begin
  if KeyComp('boolean') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func76: TtkTokenKind;
begin
  if KeyComp('default') then Result := tkKey else
    if KeyComp('const') then Result := tkKey else
      if KeyComp('module') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func77: TtkTokenKind;
begin
  if KeyComp('raises') then Result := tkKey else
    if KeyComp('native') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func78: TtkTokenKind;
begin
  if KeyComp('union') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func84: TtkTokenKind;
begin
  if KeyComp('inout') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func85: TtkTokenKind;
begin
  if KeyComp('short') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func88: TtkTokenKind;
begin
  if KeyComp('switch') then Result := tkKey else
    if KeyComp('typedef') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func89: TtkTokenKind;
begin
  if KeyComp('oneway') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func90: TtkTokenKind;
begin
  if KeyComp('interface') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func92: TtkTokenKind;
begin
  if KeyComp('abstract') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func93: TtkTokenKind;
begin
  if KeyComp('string') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func95: TtkTokenKind;
begin
  if KeyComp('factory') then Result := tkKey else
    if KeyComp('ValueBase') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func97: TtkTokenKind;
begin
  if KeyComp('custom') then Result := tkKey else
    if KeyComp('sequence') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func98: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func101: TtkTokenKind;
begin
  if KeyComp('unsigned') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func102: TtkTokenKind;
begin
  if KeyComp('readonly') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func107: TtkTokenKind;
begin
  if KeyComp('struct') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func108: TtkTokenKind;
begin
  if KeyComp('context') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func117: TtkTokenKind;
begin
  if KeyComp('wstring') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynIdlSyn.Func120: TtkTokenKind;
begin
  if KeyComp('exception') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func125: TtkTokenKind;
begin
  if KeyComp('attribute') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func128: TtkTokenKind;
begin
  if KeyComp('truncatable') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func136: TtkTokenKind;
begin
  if KeyComp('valuetype') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.Func152: TtkTokenKind;
begin
  if KeyComp('supports') then Result := tkKey else Result := tkIdentifier;
end;

function TSynIdlSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynIdlSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynIdlSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0       : fProcTable[I] := NullProc;
      #10      : fProcTable[I] := LFProc;
      #13      : fProcTable[I] := CRProc;
      '/'      : fProcTable[I] := CommentOpenProc;
      '"'      : fProcTable[I] := StringOpenProc;
      ''''     : fProcTable[I] := CharOpenProc;
      '#'      : fProcTable[I] := PreProcessorProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      'A'..'Z',
      'a'..'z',
      '_'      : fProcTable[I] := IdentProc;
      '0'..'9' : fProcTable[I] := NumberProc;
      '-', '+',
      '*', '\',
      ',', '.',
      '[', ']',
      '{', '}',
      '<', '>',
      '(', ')',
      '=', '?',
      ':', ';' : fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynIdlSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynIdlSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynIdlSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.': if FLine[Run + 1] = '.' then
             Break;
    end;
    inc(Run);
  end;
end; { NumberProc }


procedure TSynIdlSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynIdlSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynIdlSyn.CommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '*') then
  begin
    fRange := rsComment;
    CommentProc;
    fTokenID := tkComment;
  end
  else if (fLine[Run] = '/') then
  begin
    while not (fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
    fTokenID := tkComment;
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynIdlSyn.CommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and
           (fLine[Run + 1] = '/') then
        begin
          Inc(Run, 2);
          fRange := rsUnKnown;
          Break;
        end;
        if not (fLine[Run] in [#0, #10, #13]) then
          Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynIdlSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynIdlSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '"') then
    begin
      Inc(Run);
      fRange := rsUnKnown;
      Break;
    end
    else if (fLine[Run] = '\') then
      Inc(Run);
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynIdlSyn.CharOpenProc;
begin
  Inc(Run);
  fRange := rsChar;
  CharProc;
  fTokenID := tkString;
end;

procedure TSynIdlSyn.CharProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '''') then
    begin
      Inc(Run);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynIdlSyn.PreProcessorProc;
var
  Directive: String;
begin
  Directive := '';
  while not (fLine[Run] in [#0, #9, #10, #13, #32]) do
  begin
    Directive := Directive + fLine[Run];
    Inc(Run);
  end;
  if (AnsiCompareStr(Directive, '#include') = 0) then
    fTokenID := tkPreprocessor
  else if (AnsiCompareStr(Directive, '#pragma') = 0) then
    fTokenID := tkPreprocessor
  else
    fTokenID := tkIdentifier;
end; { PreProcessorProc }


constructor TSynIdlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fDatatypeAttri := TSynHighLighterAttributes.Create(SYNS_AttrDatatype);
  fDatatypeAttri.Style := [fsBold];
  fDatatypeAttri.Foreground := clTeal;
  AddAttribute(fDatatypeAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fPreprocessorAttri := TSynHighLighterAttributes.Create(SYNS_AttrPreprocessor);
  fPreprocessorAttri.Foreground := clRed;
  AddAttribute(fPreprocessorAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCORBAIDL;
  fRange := rsUnknown;
end;

procedure TSynIdlSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynIdlSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynIdlSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynIdlSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynIdlSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: CommentProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynIdlSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
    SYN_ATTR_SYMBOL     : Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynIdlSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynIdlSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynIdlSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynIdlSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDatatype: Result := fDatatypeAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynIdlSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynIdlSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynIdlSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z'] + TSynSpecialChars;
end;

function TSynIdlSyn.GetSampleSource: string;
begin
  Result := '/* CORBA IDL sample source */'#13#10 +
            '#include <sample.idl>'#13#10 +
            #13#10 +
            'const string TestString = "Hello World";'#13#10 +
            'const long TestLong = 10;'#13#10 +
            #13#10 +
            'module TestModule {'#13#10 +
            '  interface DemoInterface {'#13#10 +
            '    boolean HelloWorld(in string Message);'#13#10 +
            '  }'#13#10 +
            '}';
end;

function TSynIdlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCORBAIDL;
end;

class function TSynIdlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCORBAIDL;
end;

procedure TSynIdlSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynIdlSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynIdlSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynIdlSyn);
{$ENDIF}
end.
