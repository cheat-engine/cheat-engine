{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterUNIXShellScript.pas, released 2001-11-13.
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

$Id: SynHighlighterUNIXShellScript.pas,v 1.8 2005/01/28 16:53:25 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a UNIX Shell Script highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(10 November 2001)
@lastmod(2001-11-13)
The SynHighlighterUNIXShellScript unit provides SynEdit with a UNIX Shell Script highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERUNIXSHELLSCRIPT}
unit SynHighlighterUNIXShellScript;
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
    tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

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
  TSynUNIXShellScriptSyn = class(TSynCustomHighlighter)
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
    fVarAttri: TSynHighlighterAttributes;
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
    procedure DollarProc;
    procedure DotProc;
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
    property VarAttri: TSynHighlighterAttributes read fVarAttri
      write fVarAttri;
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
  ShellScriptKeysCount = 110;
  ShellScriptKeys: array[1..ShellScriptKeysCount] of string = (
    'AWK', 'BANNER', 'BASENAME', 'BDIFF', 'BG', 'BREAK', 'CASE', 'CAT', 'CC',
    'CD', 'CHDIR', 'CHGRP', 'CHMOD', 'CHOWN', 'CLEAR', 'COMPRESS', 'CONTINUE',
    'CP', 'CPIO', 'CUT', 'DATE', 'DD', 'DIFF', 'DF', 'DO', 'DONE', 'DTPAD',
    'ECHO', 'ELIF', 'ELSE', 'ESAC', 'EVAL', 'EXIT', 'EXPORT', 'EXPR', 'FG', 'FI',
    'FINGER', 'FOLD', 'FOR', 'FTP', 'G++', 'GCC', 'GETOPTS', 'GREP', 'GZIP',
    'HEAD', 'HASH', 'IF', 'IN', 'JOBS', 'KILL', 'LD', 'LOGIN', 'LN', 'LS',
    'MAKE', 'MKDIR', 'MT', 'MV', 'NEWGRP', 'NOHUP', 'PASTE', 'PING', 'PERL',
    'PG', 'PR', 'PS', 'PWD', 'OD', 'RCP', 'READ', 'REMSH', 'RETURN', 'RM', 'RSH',
    'RWHO', 'SED', 'SET', 'SH', 'SHIFT', 'STOP', 'STRINGS', 'STRIP', 'SYNC',
    'TAIL', 'TAR', 'TELNET', 'TEST', 'TIMES', 'THEN', 'TPUT', 'TRAP', 'TRUE',
    'TTY', 'TYPE', 'ULIMIT', 'UMASK', 'UNSET', 'UNTIL', 'UUDECODE', 'UUENCODE',
    'VI', 'WAIT', 'WC', 'WHILE', 'WHO', 'XTERN', 'ZCAT', 'ZIP');
  ShellScriptSecondKeysCount = 23;
  ShellScriptSecondKeys: array[1..ShellScriptSecondKeysCount] of string = (
    'CDPATH', 'EDITOR', 'HOME', 'IFS', 'LANG', 'LC_TYPE', 'LC_MESSAGES',
    'LD_LIBRARY_PATH', 'LOGNAME', 'MAIL', 'MAILCHECK', 'MAILPATH', 'MANPATH',
    'PATH', 'PS1', 'PS2', 'PWD', 'SHACCT', 'SHELL', 'SHLIB_PATH', 'TERM',
    'TERMCAP', 'TZ');

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

function TSynUNIXShellScriptSyn.IsKeyword(const AKeyword: string): boolean;
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

function TSynUNIXShellScriptSyn.IsSecondKeyWord(aToken: String): Boolean;
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

procedure TSynUNIXShellScriptSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '<': fProcTable[I] := LowerProc;
      '#': fProcTable[I] := SlashProc;
      '{': fProcTable[I] := BraceOpenProc;
      ';': fProcTable[I] := PointCommaProc;
      '.': fProcTable[i] := DotProc;
      #13: fProcTable[I] := CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '(': fProcTable[I] := RoundOpenProc;
      '/': fProcTable[I] := SlashProc;
      '$': fProcTable[i] := DollarProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      #34, #39: fProcTable[I] := StringProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynUNIXShellScriptSyn.Create(AOwner: TComponent);
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
  if not (csDesigning in ComponentState) then begin
    for i := 1 to ShellScriptKeysCount do
      fKeyWords.Add(ShellScriptKeys[i]);
    for i := 1 to ShellScriptSecondKeysCount do
      fSecondKeys.Add(ShellScriptSecondKeys[i]);
  end;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clNavy;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord);
  AddAttribute(fSecondKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clRed;
  AddAttribute(fSymbolAttri);
  fVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  fVarAttri.Foreground := clPurple;
  AddAttribute(fVarAttri);
  SetAttributesOnChange(DefHighlightChange);

  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterUNIXShellScript;
end; { Create }

destructor TSynUNIXShellScriptSyn.Destroy;
begin
  fKeyWords.Free;
  fSecondKeys.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynUNIXShellScriptSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynUNIXShellScriptSyn.DollarProc;
const
  IdentChars = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];
var
  cc: Char;
begin
  inc(Run);
  fTokenID := tkVariable;
  if FLine[Run] = #0 then Exit;
  cc := FLine[Run];
  inc(Run);
  if (cc = '{') then begin
    // ${var}
    while FLine[Run] in IdentChars do begin
      case FLine[Run] of
        #0, #10, #13: Break;
      end;
      inc(Run);
    end;
    if FLine[Run] = '}' then Inc(Run);
  end else
    // $var
    while FLine[Run] in IdentChars do
      inc(Run);
end;

procedure TSynUNIXShellScriptSyn.DotProc;
  function TestDot: boolean;
  var
    i: integer;
  begin
    result := false;
    i := run;
    inc(i);
    while (FLine[i] in ['a'..'z', 'A'..'Z']) do
      inc(i);
    if i > (run + 1) then
      result := true;
    if result then
      run := i;
  end;
begin
  // Don't highlight filenames like filename.zip
  if TestDot then
    fTokenID := tkIdentifier
  else begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynUNIXShellScriptSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynUNIXShellScriptSyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynUNIXShellScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynUNIXShellScriptSyn.IdentProc;
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

procedure TSynUNIXShellScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynUNIXShellScriptSyn.LowerProc;
{$IFDEF SYN_HEREDOC}
const
  // In UNIX Shell, Heredoc delimiter can be pretty much anything and the list
  // of alpha-numeric characters is extended with a few common special characters
  AlphaNumericChars = ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '+', '!', '#', '%'];
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
    if (FLine[Run + 2] = '-') and (FLine[Run + 3] in ['"', '''', '`', '\']) then
    begin
      SkipRun := 2;
      if FLine[Run + 3] <> '\' then
        QuoteChar := FLine[Run + 3];
    end
    else
    if (FLine[Run + 2] in ['-', '"', '''', '`', '\']) then
    begin
      SkipRun := 1;
      if not (FLine[Run + 2] in ['-', '\']) then
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

procedure TSynUNIXShellScriptSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynUNIXShellScriptSyn.NumberProc;
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

procedure TSynUNIXShellScriptSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynUNIXShellScriptSyn.SlashProc;
begin
  if FLine[Run] = '#' then begin
    // Perl Styled Comment
    inc(Run);
    fTokenID := tkComment;
    while FLine[Run] <> #0 do
    begin
      case FLine[Run] of
        #10, #13: break;
      end;
      inc(Run);
    end;
  end
  else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynUNIXShellScriptSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynUNIXShellScriptSyn.StringProc;
var
  QuoteChar: Char;
begin
// Single and Double Quotes.

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

procedure TSynUNIXShellScriptSyn.UnknownProc;
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
procedure TSynUNIXShellScriptSyn.HeredocProc;

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

procedure TSynUNIXShellScriptSyn.Next;
begin
  fTokenPos := Run;
{$IFDEF SYN_HEREDOC}
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
  else
{$ENDIF}
    fProcTable[fLine[Run]];
end;

function TSynUNIXShellScriptSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynUNIXShellScriptSyn.GetEol: Boolean;
begin
  Result := False;
  if fTokenId = tkNull then Result := True;
end;

function TSynUNIXShellScriptSyn.GetRange: Pointer;
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

function TSynUNIXShellScriptSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynUNIXShellScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynUNIXShellScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
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
    tkVariable: Result := fVarAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynUNIXShellScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynUNIXShellScriptSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynUNIXShellScriptSyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynUNIXShellScriptSyn.SetRange(Value: Pointer);
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

procedure TSynUNIXShellScriptSyn.SetSecondKeys(const Value: TStrings);
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

function TSynUNIXShellScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterUNIXShellScript;
end;

class function TSynUNIXShellScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangNameUNIXShellScript;
end;

function TSynUNIXShellScriptSyn.GetSampleSource: string;
begin
  Result := '######################################'#13#10 +
            '# Here is a comment about some stuff #'#13#10 +
            '######################################'#13#10 +
            ''#13#10 +
            'case $BUILD_MODE in'#13#10 +
            '  full )'#13#10 +
            '      MyFirstFunction'#13#10 +
            '      ;;'#13#10 +
            '  rekit)'#13#10 +
            '      MySecondFunction'#13#10 +
            '    ;;'#13#10 +
            '  installer)'#13#10 +
            '      MyThirdFunction'#13#10 +
            '    ;;'#13#10 +
            'esac';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynUNIXShellScriptSyn);
{$ENDIF}
end.
