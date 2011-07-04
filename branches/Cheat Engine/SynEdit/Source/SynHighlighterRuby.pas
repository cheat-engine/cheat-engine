{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRuby.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
All Rights Reserved.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)
"Heredoc" syntax highlighting implementation by Marko Njezic.

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

$Id: SynHighlighterRuby.pas,v 1.11 2005/01/28 16:53:25 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Ruby highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(21 May 2001)
@lastmod(2001-11-13)
The SynHighlighterVisualLisp unit provides SynEdit with a Ruby highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERRUBY}
unit SynHighlighterRuby;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkSpace, tkString, tkSymbol, tkUnknown);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsHeredoc, rsIndentedHeredoc);

  TRangePointer = packed record
    case Boolean of
      True  : (Ptr: Pointer);
      False : (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown);
{$ENDIF}

  TProcTableProc = procedure of object;

type
  TSynRubySyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength : Byte;
    fHeredocChecksum : Word;
{$ENDIF}
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fLineNumber: Integer;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fSecondKeys: TStrings;

    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure MakeMethodTables;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
    procedure SetSecondKeys(const Value: TStrings);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: string): boolean; override;
    function IsSecondKeyWord(aToken: string): Boolean;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
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
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri
      write fSecondKeyAttri;
    property SecondKeyWords: TStrings read fSecondKeys write SetSecondKeys;
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
  QSynEditMiscProcs,
  QSynEditStrConst;
{$ELSE}
  SynEditMiscProcs,
  SynEditStrConst;
{$ENDIF}

const
  RubyKeysCount = 43;
  RubyKeys: array[1..RubyKeysCount] of string = (
    'ALIAS', 'ATTR', 'BEGIN', 'BREAK', 'CASE', 'CLASS', 'DEF', 'DO', 'ELSE',
    'ELSIF', 'END', 'ENSURE', 'EXIT', 'EXTEND', 'FALSE', 'FOR', 'GETS', 'IF',
    'IN', 'INCLUDE', 'LOAD', 'LOOP', 'MODULE', 'NEXT', 'NIL', 'NOT', 'PRINT',
    'PRIVATE', 'PUBLIC', 'PUTS', 'RAISE', 'REDO', 'REQUIRE', 'RESCUE', 'RETRY',
    'RETURN', 'SELF', 'THEN', 'TRUE', 'UNLESS', 'WHEN', 'WHILE', 'YIELD');

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
    case I in ['_', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

function TSynRubySyn.IsKeyword(const AKeyword: string): boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fKeywords.Count - 1;
  Result := False;
  Token := UpperCase(AKeyword);

  while First <= Last do begin
    I := (First + Last) shr 1;
    Compare := CompareStr(fKeywords[I], Token);
    if Compare = 0 then begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }

function TSynRubySyn.IsSecondKeyWord(aToken: String): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fSecondKeys.Count - 1;
  Result := False;
  Token := UpperCase(aToken);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(fSecondKeys[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsSecondKeyWord }

procedure TSynRubySyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '<': fProcTable[I] := LowerProc;
      '#': fProcTable[I] := SlashProc;
      '{': fProcTable[I] := BraceOpenProc;
      ';': fProcTable[I] := PointCommaProc;
      #13: fProcTable[I] := CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '(': fProcTable[I] := RoundOpenProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      #34, #39: fProcTable[I] := StringProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynRubySyn.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);
  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fSecondKeys := TStringList.Create;
  TStringList(fSecondKeys).Sorted := True;
  TStringList(fSecondKeys).Duplicates := dupIgnore;
  if not (csDesigning in ComponentState) then
    for i := 1 to RubyKeysCount do
      fKeyWords.Add(RubyKeys[i]);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clMaroon;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord);
  AddAttribute(fSecondKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clGreen;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clPurple;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clBlue;
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterRuby;
end; { Create }

destructor TSynRubySyn.Destroy;
begin
  fKeyWords.Free;
  fSecondKeys.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynRubySyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynRubySyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynRubySyn.IdentProc;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  if IsKeyWord(GetToken) then begin
    fTokenId := tkKey;
    Exit;
  end
  else fTokenId := tkIdentifier;
  if IsSecondKeyWord(GetToken)
    then fTokenId := tkSecondKey
    else fTokenId := tkIdentifier;
end;

procedure TSynRubySyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynRubySyn.LowerProc;
{$IFDEF SYN_HEREDOC}
const
  AlphaNumericChars = ['A'..'Z', 'a'..'z', '0'..'9', '_'];
var
  i, Len, SkipRun : Integer;
  IndentedHeredoc : Boolean;
  QuoteChar : Char;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  if FLine[Run + 1] = '<' then
  begin
    fTokenID := tkSymbol;

    SkipRun := 0;
    QuoteChar := #0;
    if (FLine[Run + 2] = '-') and (FLine[Run + 3] in ['"', '''', '`']) then
    begin
      SkipRun := 2;
      QuoteChar := FLine[Run + 3];
    end
    else
    if (FLine[Run + 2] in ['-', '"', '''', '`']) then
    begin
      SkipRun := 1;
      if FLine[Run + 2] <> '-' then
        QuoteChar := FLine[Run + 2];
    end;
    IndentedHeredoc := (SkipRun > 0) and (FLine[Run + 2] = '-');

    if (FLine[Run + SkipRun + 2] in AlphaNumericChars) then
    begin
      inc(Run, 2);

      i := Run;
      while FLine[SkipRun + i] in AlphaNumericChars do Inc(i);
      Len := i - Run;

      if Len > 255 then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if (QuoteChar <> #0) and (FLine[Run + SkipRun + Len] <> QuoteChar) then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if IndentedHeredoc then
        fRange := rsIndentedHeredoc
      else
        fRange := rsHeredoc;
      fHeredocLength := Len;
      fHeredocChecksum := CalcFCS(FLine[Run + SkipRun], Len);

      Inc(Run, SkipRun + Len);
      fTokenID := tkString;
    end
    else
      inc(Run, 2);
  end
  else
{$ENDIF}
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynRubySyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynRubySyn.NumberProc;
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

procedure TSynRubySyn.RoundOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynRubySyn.SlashProc;
begin
  case FLine[Run] of
    '/':
      begin
        inc(Run);
        fTokenId := tkSymbol;
      end;
    '*':
      begin
        inc(Run);
        fTokenId := tkSymbol;
      end;
  else
    begin
      fTokenID := tkComment;
      while FLine[Run] <> #0 do
      begin
        case FLine[Run] of
          #10, #13: break;
        end;
        inc(Run);
      end;
    end;
  end;
end;

procedure TSynRubySyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynRubySyn.StringProc;
var
  QuoteChar: Char;
begin
// Ha, ha, Strings in Ruby (could be anything)!!!!

//There are three more ways to construct string literals: %q, %Q, and ``here
//documents.''
//
//%q and %Q start delimited single- and double-quoted strings.
//
//%q/general single-quoted string/ » general single-quoted string
//%Q!general double-quoted string! » general double-quoted string
//%Q{Seconds/day: #{24*60*60}}     » Seconds/day: 86400
//
//The character following the ``q'' or ``Q'' is the delimiter. If it is an
//opening bracket, brace, parenthesis, or less-than sign, the string is read
//until the matching close symbol is found. Otherwise the string is read until
//the next occurrence of the same delimiter.

  fTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar)
    then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = QuoteChar;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynRubySyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynRubySyn.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[Run] of
       #0 : NullProc;
      #10 : LFProc;
      #13 : CRProc;
    else
      repeat
        inc(Run);
      until FLine[Run] in [#0, #10, #13];
    end;
  end;

var
  i : Integer;
begin
  if (FLine[Run] in [#0, #10, #13]) and (fTokenPos = Run) then
  begin
    fProcTable[ FLine[Run] ];
    Exit;
  end;
  fTokenID := tkString;

  if fRange = rsIndentedHeredoc then
    while FLine[Run] in [#9, #32] do Inc(Run);

  if ((Run = 0) and (fRange = rsHeredoc)) or (fRange = rsIndentedHeredoc) then
  begin
    i := 0;

    while not (FLine[Run + i] in [#0, #10, #13]) do
    begin
      if i > fHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(i);
    end;

    if i <> fHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[Run], i) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := Run + i;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynRubySyn.Next;
begin
  fTokenPos := Run;
{$IFDEF SYN_HEREDOC}
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
  else
{$ENDIF}
    fProcTable[fLine[Run]];
end;

function TSynRubySyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynRubySyn.GetEol: Boolean;
begin
  Result := False;
  if fTokenId = tkNull then Result := True;
end;

function TSynRubySyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer : TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynRubySyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynRubySyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynRubySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynRubySyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynRubySyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynRubySyn.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer : TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  fRange := TRangeState(RangePointer.Range);
  fHeredocLength := 0;
  fHeredocChecksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

procedure TSynRubySyn.SetSecondKeys(const Value: TStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := UpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynRubySyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterRuby;
end;

class function TSynRubySyn.GetLanguageName: string;
begin
  Result := SYNS_LangRuby;
end;

function TSynRubySyn.GetSampleSource: string;
begin
  Result :=
    '# Factorial'+#13#10+
    'def fact(n)'+#13#10+
    '  if n == 0'+#13#10+
    '    1'+#13#10+
    '  else'+#13#10+
    '    n * fact(n-1)'+#13#10+
    '  end'+#13#10+
    'end'+#13#10+
    'print fact(ARGV[0].to_i), "\n"';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynRubySyn);
{$ENDIF}
end.
