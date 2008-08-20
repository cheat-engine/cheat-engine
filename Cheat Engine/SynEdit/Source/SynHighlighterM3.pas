{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterM3.pas, released 2000-11-23.

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

$Id: SynHighlighterM3.pas,v 1.12 2005/01/28 16:53:24 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Modula-3 syntax highlighter for SynEdit)
@author(Martin Pley <synedit@pley.de>)
@created(January 2000, converted to SynEdit November 23, 2000)
@lastmod(2000-11-23)
The SynHighlighterM3 unit provides SynEdit with a Modula-3 (.m3) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERM3}
unit SynHighlighterM3;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
{$ELSE}
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkPragma,
    tkReserved, tkSpace, tkString, tkSymbol, tkUnknown, tkSyntaxError);

  TTokenRange = (trNone, trComment, trPragma);

  TRangeState = packed record
    case boolean of
      FALSE: (p: pointer);
      TRUE: (TokenRange: word; Level: word);
    end;

  TProcTableProc = procedure of object;

  TSynM3Syn = class(TSynCustomHighLighter)
  private
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fRange: TRangeState;
    fStringLen: integer;
    fToIdent: PChar;
    fTokenPos: integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPragmaAttri: TSynHighlighterAttributes;
    fReservedAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSyntaxErrorAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function IdentKind(MayBe: PChar): TtkTokenKind;
    function KeyComp(AKey: string): boolean;
    function KeyHash(ToHash: PChar): integer;
    procedure MakeMethodTables;
    procedure SymAsciiCharProc;
    procedure SymCommentHelpProc;
    procedure SymCRProc;
    procedure SymIdentProc;
    procedure SymLFProc;
    procedure SymNestedHelperProc(AOpenChar, ACloseChar: char);
    procedure SymNullProc;
    procedure SymNumberProc;
    procedure SymPragmaProc;
    procedure SymPragmaHelpProc;
    procedure SymRoundOpenProc;
    procedure SymSpaceProc;
    procedure SymStringProc;
    procedure SymSymbolProc;
    procedure SymUnknownProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  public
    property _Keywords: TSynHashEntryList read fKeywords;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PragmaAttri: TSynHighlighterAttributes read fPragmaAttri
      write fPragmaAttri;
    property ReservedAttri: TSynHighlighterAttributes read fReservedAttri
      write fReservedAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SyntaxErrorAttri: TSynHighlighterAttributes read fSyntaxErrorAttri
      write fSyntaxErrorAttri;
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

const
  Keywords: string =
    'AS,AND,ANY,ARRAY,BEGIN,BITS,BRANDED,BY,CASE,CONST,DIV,DO,ELSE,ELSIF,END,' +
    'EVAL,EXCEPT,EXCEPTION,EXIT,EXPORTS,FINALLY,FOR,FROM,GENERIC,IF,IMPORT,' +
    'IN,INTERFACE,LOCK,LOOP,METHODS,MOD,MODULE,NOT,OBJECT,OF,OR,OVERRIDES,' +
    'PROCEDURE,RAISE,RAISES,READONLY,RECORD,REF,REPEAT,RETURN,REVEAL,ROOT,' +
    'SET,THEN,TO,TRY,TYPE,TYPECASE,UNSAFE,UNTIL,UNTRACED,VALUE,VAR,WHILE,WITH';

  ReservedWords: string =
    'ABS,ADDRESS,ADR,ADRSIZE,BITSIZE,BOOLEAN,BYTESIZE,CARDINAL,CEILING,CHAR,' +
    'DEC,DISPOSE,FALSE,FIRST,FLOAT,FLOOR,INC,INTEGER,ISTYPE,LAST,LONGFLOAT,' +
    'LONGREAL,LOOPHOLE,MAX,MIN,MUTEX,NARROW,NEW,NIL,NULL,NUMBER,ORD,REAL,' +
    'REFANY,ROUND,SUBARRAY,TEXT,TRUE,TRUNC,TYPECODE,VAL';

procedure MakeIdentTable;
var
  I: Char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for I := 'a' to 'z' do
    Identifiers[i] := TRUE;
  for I := 'A' to 'Z' do
    Identifiers[i] := TRUE;
  for I := '0' to '9' do
    Identifiers[i] := TRUE;
  Identifiers['_'] := TRUE;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  for I := 'a' to 'z' do
    mHashTable[I] := 1 + Ord(I) - Ord('a');
  for I := 'A' to 'Z' do
    mHashTable[I] := 1 + Ord(I) - Ord('A');
  mHashTable['_'] := 27;
  for I := '0' to '9' do
    mHashTable[I] := 28 + Ord(I) - Ord('0');
end;

procedure TSynM3Syn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynM3Syn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

function TSynM3Syn.KeyComp(AKey: string): boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if pKey1^ <> pKey2^ then begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := TRUE;
end;

function TSynM3Syn.KeyHash(ToHash: PChar): integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
{$IFOPT Q-}
    Result := 7 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (7 * Result + mHashTable[ToHash^]) and $FFFFFF;
{$ENDIF}
    Inc(ToHash);
  end;
  Result := Result and $FF; // 255
  fStringLen := ToHash - fToIdent;
end;

procedure TSynM3Syn.MakeMethodTables;
var
  I: char;
begin
  for I := #0 to #255 do
    case I of
      #39: fProcTable[I] := SymAsciiCharProc;
      #13: fProcTable[I] := SymCRProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := SymIdentProc;
      #10: fProcTable[I] := SymLFProc;
       #0: fProcTable[I] := SymNullProc;
      '0'..'9':
        fProcTable[I] := SymNumberProc;
      '(': fProcTable[I] := SymRoundOpenProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SymSpaceProc;
      '{','}','|','!', #35..#38, #42..#47, #58, #59, #61..#64, #91..#94, ')':
        fProcTable[I] := SymSymbolProc;
      '<' : fProcTable[I]:= SymPragmaProc;
      #34: fProcTable[I] := SymStringProc;
    else
      fProcTable[I] := SymUnknownProc;
    end;
end;

constructor TSynM3Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fPragmaAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor);
  fPragmaAttri.Style:= [fsBold];
  AddAttribute(fPragmaAttri);
  fReservedAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  AddAttribute(fReservedAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fSyntaxErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError);
  fSyntaxErrorAttri.Foreground := clRed;
  AddAttribute(fSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);

  MakeMethodTables;
  EnumerateKeywords(Ord(tkKey), Keywords, IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkReserved), ReservedWords, IdentChars, DoAddKeyword);
  fDefaultFilter := SYNS_FilterModula3;
end;

destructor TSynM3Syn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynM3Syn.SymAsciiCharProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do begin
    case fLine[Run] of
      '\': if fLine[Run + 1] = #39 then
             Inc(Run);
      #39: begin
             Inc(Run);
             if fLine[Run] <> #39 then
               break;
           end;
    end;
    Inc(Run);
  end;
end;

procedure TSynM3Syn.SymCommentHelpProc;
begin
  fTokenID := tkComment;
  SymNestedHelperProc('(', ')');
end;

procedure TSynM3Syn.SymCRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynM3Syn.SymIdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  Inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynM3Syn.SymLFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynM3Syn.SymNestedHelperProc(AOpenChar, ACloseChar: char);
begin
  case fLine[Run] of
     #0: SymNullProc;
    #10: SymLFProc;
    #13: SymCRProc;
  else
    repeat
      if fLine[Run]= AOpenChar then begin
        Inc(Run);
        if fLine[Run] = '*' then begin
          Inc(Run);
          Inc(fRange.Level);
        end;
      end else if fLine[Run] = '*' then begin
        Inc(Run);
        if fLine[Run] = ACloseChar then begin
          Inc(Run);
          if fRange.Level > 0 then
            Dec(fRange.Level);
          if fRange.Level = 0 then begin
            fRange.TokenRange := Ord(trNone);
            break
          end;
        end;
      end else
        Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynM3Syn.SymNullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynM3Syn.SymNumberProc;
const
  Digits: array[0..15] of char = '0123456789abcdef';
var
  BasedNumber: boolean;
  i, MaxDigit: integer;
  ValidDigits: TSynIdentChars;
begin
  fTokenID := tkNumber;
  BasedNumber := FALSE;
  MaxDigit := 9;
  // skip leading zeros, but they can be numbers too
  while fLine[Run] = '0' do
    Inc(Run);
  if not Identifiers[fLine[Run]] then
    exit;
  // check for numbers with a base prefix
  if (fLine[Run] in ['2'..'9']) and (fLine[Run + 1] = '_') then begin
    BasedNumber := TRUE;
    MaxDigit := Ord(fLine[Run]) - Ord('0') - 1;
    Inc(Run, 2);
  end else if (fLine[Run] ='1') and (fLine[Run + 1] in ['0'..'6'])
    and (fLine[Run + 2] = '_')
  then begin
    BasedNumber := TRUE;
    MaxDigit := 10 + Ord(fLine[Run + 1]) - Ord('0') - 1;
    Inc(Run, 3);
  end;
  if BasedNumber then begin
    ValidDigits := [];
    i := MaxDigit;
    while i >= 10 do begin
      Include(ValidDigits, Digits[i]);
      Include(ValidDigits, UpCase(Digits[i]));
      Dec(i);
    end;
    while i >= 0 do begin
      Include(ValidDigits, Digits[i]);
      Dec(i);
    end;
    // advance over all valid digits, but at least one has to be there
    if fLine[Run] in ValidDigits then begin
      repeat
        Inc(Run);
      until not (fLine[Run] in ValidDigits);
    end else
      fTokenID := tkSyntaxError;
  end else begin
    // "normal" numbers
    repeat
      Inc(Run);
    until not (fLine[Run] in ['0'..'9']);
    // can include a decimal point and an exponent
    if fLine[Run] = '.' then begin
      Inc(Run);
      if fLine[Run] in ['0'..'9'] then begin
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9']);
      end else
        fTokenID := tkSyntaxError; // must be a number after the '.'
    end;
    // can include an exponent
    if fLine[Run] in ['d', 'D', 'e', 'E', 'x', 'X'] then begin
      Inc(Run);
      if fLine[Run] in ['+', '-'] then
        Inc(Run);
      if fLine[Run] in ['0'..'9'] then begin
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9']);
      end else // exponent must include a number
        fTokenID := tkSyntaxError;
    end;
  end;
  // it's a syntax error if there are any Identifier chars left
  if Identifiers[fLine[Run]] then begin
    fTokenID := tkSyntaxError;
    repeat
      Inc(Run);
    until not Identifiers[fLine[Run]];
  end;
end;

procedure TSynM3Syn.SymPragmaProc;
begin
  Inc(Run);
  if fLine[Run] = '*' then begin
    Inc(Run);
    fRange.TokenRange := Ord(trPragma);
    Inc(fRange.Level);
    if fLine[Run] in [#0, #10, #13] then
      fTokenID := tkPragma
    else
      SymPragmaHelpProc;
  end else
    fTokenID := tkSymbol;
end;

procedure TSynM3Syn.SymPragmaHelpProc;
begin
  fTokenID := tkPragma;
  SymNestedHelperProc('<', '>');
end;

procedure TSynM3Syn.SymRoundOpenProc;
begin
  Inc(Run);
  if fLine[Run] = '*' then begin
    Inc(Run);
    fRange.TokenRange := Ord(trComment);
    Inc(fRange.Level);
    if fLine[Run] in [#0, #10, #13] then
      fTokenID := tkComment
    else
      SymCommentHelpProc;
  end else begin
    fTokenID := tkSymbol;
    if fLine[Run] = '.' then
      Inc(Run);
  end;
end;

procedure TSynM3Syn.SymSpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynM3Syn.SymStringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do begin
    case fLine[Run] of
      #34: begin
             Inc(Run);
             break;
           end;
      '\': if fLine[Run + 1] in [#34, '\'] then
             Inc(Run);
    end;
    Inc(Run);
  end;
end;

procedure TSynM3Syn.SymSymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynM3Syn.SymUnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynM3Syn.Next;
begin
  fTokenPos := Run;
  case TTokenRange(fRange.TokenRange) of
    trComment: SymCommentHelpProc;
    trPragma: SymPragmaHelpProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynM3Syn.GetDefaultAttribute(Index: integer):
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

function TSynM3Syn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynM3Syn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynM3Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterModula3;
end;

class function TSynM3Syn.GetLanguageName: string;
begin
  Result := SYNS_LangModula3;
end;

function TSynM3Syn.GetRange: pointer;
begin
  result := fRange.p;
end;

function TSynM3Syn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, fLine + fTokenPos, Len);
end;

function TSynM3Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPragma: Result:= fPragmaAttri;
    tkReserved: Result := fReservedAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSyntaxError: Result := fSyntaxErrorAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynM3Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynM3Syn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynM3Syn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

procedure TSynM3Syn.ResetRange;
begin
  fRange.p := nil;
end;

procedure TSynM3Syn.SetLine(NewValue: string; LineNumber: integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynM3Syn.SetRange(Value: pointer);
begin
  fRange.p := Value;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynM3Syn);
{$ENDIF}
end.
