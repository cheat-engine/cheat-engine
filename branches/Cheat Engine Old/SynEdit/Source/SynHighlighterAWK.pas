{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterAWK.pas, released 2000-06-18.
The Original Code is based on the hkAWKSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Hideo Koiso.
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

$Id: SynHighlighterAWK.pas,v 1.10 2004/07/13 00:00:29 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a AWK Script highlighter for SynEdit)
@author(Hideo Koiso <sprhythm@fureai.or.jp>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(7 November 1999, converted to SynEdit April 18, 2000)
@lastmod(June 19, 2000)
The SynHighlighterAWK unit provides SynEdit with a AWK Script (.awk) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERAWK}
unit SynHighlighterAWK;
{$ENDIF}

interface

{$I SynEdit.inc}

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
  TtkTokenKind = (tkComment, tkIdentifier, tkInterFunc, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkSysVar, tkUnknown);

  TProcTableProc = procedure of object;

  TSynAWKSyn = class(TSynCustomHighLighter)
  private
    AWKSyntaxList: TStringList;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: Longint;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInterFuncAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSysVarAttri: TSynHighlighterAttributes;
    fLineNumber: Integer;
    procedure AndProc;
    procedure CommentProc;
    procedure CRProc;
    procedure ExclamProc;
    procedure FieldRefProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MakeMethodTables;
    procedure MakeSyntaxList;
    procedure MinusProc;
    procedure NullProc;
    procedure OpInputProc;
    procedure OrProc;
    procedure PlusProc;
    procedure QuestionProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure NumberProc;
    procedure BraceProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InterFuncAttri: TSynHighlighterAttributes read fInterFuncAttri
      write fInterFuncAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SysVarAttri: TSynHighlighterAttributes read fSysVarAttri
      write fSysVarAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

procedure TSynAWKSyn.MakeSyntaxList;
begin
  with AWKSyntaxList do begin
    Sorted := True;

    { *** Preferably sort and put previously. *** }
    AddObject('ARGC', TObject(tkSysVar));
    AddObject('ARGIND', TObject(tkSysVar)); { GNU Extention }
    AddObject('ARGV', TObject(tkSysVar));
    AddObject('atan2', TObject(tkInterFunc));
    AddObject('BEGIN', TObject(tkKey));
    AddObject('break', TObject(tkKey));
    AddObject('close', TObject(tkInterFunc));
    AddObject('continue', TObject(tkKey));
    AddObject('CONVFMT', TObject(tkSysVar)); { POSIX Extention }
    AddObject('cos', TObject(tkInterFunc));
    AddObject('delete', TObject(tkInterFunc));
    AddObject('do', TObject(tkKey));
    AddObject('else', TObject(tkKey));
    AddObject('END', TObject(tkKey));
    AddObject('ENVIRON', TObject(tkSysVar));
    AddObject('ERRNO', TObject(tkSysVar)); { GNU Extention }
    AddObject('exit', TObject(tkKey));
    AddObject('exp', TObject(tkInterFunc));
    AddObject('FIELDWIDTH', TObject(tkSysVar)); { GNU Extention }
    AddObject('FILENAME', TObject(tkSysVar));
    AddObject('FNR', TObject(tkSysVar));
    AddObject('for', TObject(tkKey));
    AddObject('FS', TObject(tkSysVar));
    AddObject('function', TObject(tkKey));
    AddObject('getline', TObject(tkKey));
    AddObject('gsub', TObject(tkInterFunc));
    AddObject('if', TObject(tkKey));
    AddObject('IGNORECASE', TObject(tkSysVar));
    AddObject('index', TObject(tkInterFunc));
    AddObject('int', TObject(tkInterFunc));
    AddObject('jindex', TObject(tkInterFunc)); { jgawk }
    AddObject('jlength', TObject(tkInterFunc)); { jgawk }
    AddObject('jsubstr', TObject(tkInterFunc)); { jgawk }
    AddObject('length', TObject(tkInterFunc));
    AddObject('log', TObject(tkInterFunc));
    AddObject('match', TObject(tkInterFunc));
    AddObject('next', TObject(tkUnknown)); { & next file (GNU Extention) }
    AddObject('NF', TObject(tkSysVar));
    AddObject('NR', TObject(tkSysVar));
    AddObject('OFMT', TObject(tkSysVar));
    AddObject('OFS', TObject(tkSysVar));
    AddObject('ORS', TObject(tkSysVar));
    AddObject('print', TObject(tkKey));
    AddObject('printf', TObject(tkInterFunc));
    AddObject('rand', TObject(tkInterFunc));
    AddObject('return', TObject(tkKey));
    AddObject('RLENGTH', TObject(tkSysVar));
    AddObject('RS', TObject(tkSysVar));
    AddObject('RSTART', TObject(tkSysVar));
    AddObject('sin', TObject(tkInterFunc));
    AddObject('split', TObject(tkInterFunc));
    AddObject('sprintf', TObject(tkInterFunc));
    AddObject('sqrt', TObject(tkInterFunc));
    AddObject('srand', TObject(tkInterFunc));
    AddObject('strftime', TObject(tkInterFunc)); { GNU Extention }
    AddObject('sub', TObject(tkInterFunc));
    AddObject('SUBSEP', TObject(tkSysVar));
    AddObject('substr', TObject(tkInterFunc));
    AddObject('system', TObject(tkInterFunc));
    AddObject('systime', TObject(tkInterFunc)); { GNU Extention }
    AddObject('tolower', TObject(tkInterFunc));
    AddObject('toupper', TObject(tkInterFunc));
    AddObject('while', TObject(tkKey));
  end;
end;

procedure TSynAWKSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do begin
    case i of
      #0:
        fProcTable[i] := NullProc;
      #10:
        fProcTable[i] := LFProc;
      #13:
        fProcTable[i] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[i] := SpaceProc;
      '"', #$27:
        fProcTable[i] := StringProc; { "..." }
      '(', ')', '[', ']':
        fProcTable[i] := BraceProc; { (, ), [ and ] }
      '#':
        fProcTable[i] := CommentProc; { # ... }
      '$':
        fProcTable[i] := FieldRefProc; { $0 .. $9 }
      '+':
        fProcTable[i] := PlusProc; { +, ++ and += }
      '-':
        fProcTable[i] := MinusProc; { -, -- and -= }
      '!':
        fProcTable[i] := ExclamProc; { ! and !~ }
      '?':
        fProcTable[i] := QuestionProc; { ?: }
      '|':
        fProcTable[i] := OrProc; { || }
      '&':
        fProcTable[i] := AndProc; { && }
      '*', '/', '%', '^', '<', '=', '>':
        fProcTable[i] := OpInputProc; { *=, /=, %= ... etc. }
      'a'..'z', 'A'..'Z':
        fProcTable[i] := IdentProc;
      '0'..'9':
        fProcTable[i] := NumberProc;
    else
      fProcTable[i] := SymbolProc;
    end;
  end;
end;

procedure TSynAWKSyn.BraceProc;
begin
  fTokenID := tkIdentifier;
  Inc(Run);
end;

procedure TSynAWKSyn.NumberProc;
begin
  fTokenID := tkNumber;
  Inc(Run);
  while (fLine[Run] in ['0'..'9']) do begin
    Inc(Run);
  end;
end;

procedure TSynAWKSyn.IdentProc;
var
  i: Integer;
  idx: Integer;
  s: string;
begin
  i := Run;
  while (fLine[i] in ['a'..'z', 'A'..'Z']) do begin
    Inc(i);
  end;
  SetLength(s, (i - Run));
  StrLCopy(PChar(s), (fLine + Run), (i - Run));
  Run := i;
  if AWKSyntaxList.Find(s, idx) and (AWKSyntaxList.Strings[idx] = s) then begin
    fTokenID := TtkTokenKind(AWKSyntaxList.Objects[idx]);
    if (fTokenID = tkUnKnown) then begin
      fTokenID := tkKey;
      if (fLine[i] = ' ') then begin
        while (fLine[i] = ' ') do begin
          Inc(i);
        end;
        if (fLine[i + 0] = 'f') and
          (fLine[i + 1] = 'i') and
          (fLine[i + 2] = 'l') and
          (fLine[i + 3] = 'e') and
          (fLine[i + 4] in [#0..#32, ';']) then begin
          Run := (i + 4);
        end;
      end;
    end;
  end
  else begin
    fTokenID := tkIdentifier;
  end;
end;

procedure TSynAWKSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

procedure TSynAWKSyn.StringProc;
begin
  repeat
    Inc(Run);
    if (fLine[Run] = '"') and (fLine[Run - 1] <> '\') then begin
      fTokenID := tkString;
      Inc(Run);
      Exit;
    end;
  until (fLine[Run] in [#0..#31]);
  fTokenID := tkIdentifier;
end;

procedure TSynAWKSyn.CommentProc;
begin
  fTokenID := tkComment;
  while not (fLine[Run] in [#0, #10, #13]) do begin
    Inc(Run);
  end;
end;

procedure TSynAWKSyn.FieldRefProc;
begin
  Inc(Run);
  if (fLine[Run] in ['0'..'9']) and
    not (fLine[Run + 1] in ['0'..'9', 'a'..'z', 'A'..'Z']) then begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else begin
    fTokenID := tkIdentifier;
  end;
end;

procedure TSynAWKSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynAWKSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] in ['+', '=']) then begin
    Inc(Run);
  end;
end;

procedure TSynAWKSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] in ['-', '=']) then begin
    Inc(Run);
  end;
end;

procedure TSynAWKSyn.OpInputProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] = '=') then begin
    Inc(Run);
  end;
end;

procedure TSynAWKSyn.ExclamProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] in ['=', '~']) then begin
    Inc(Run);
  end;
end;

procedure TSynAWKSyn.QuestionProc;
begin
  Inc(Run);
  if (fLine[Run] = ':') then begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else begin
    fTokenID := tkIdentifier;
  end;
end;

procedure TSynAWKSyn.OrProc;
begin
  Inc(Run);
  if (fLine[Run] = '|') then begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else begin
    fTokenID := tkIdentifier;
  end;
end;

procedure TSynAWKSyn.AndProc;
begin
  Inc(Run);
  if (fLine[Run] = '&') then begin
    fTokenID := tkSymbol;
    Inc(Run);
  end
  else begin
    fTokenID := tkIdentifier;
  end;
end;

constructor TSynAWKSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clBlue;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInterFuncAttri := TSynHighlighterAttributes.Create(SYNS_AttrInternalFunction);
  fInterFuncAttri.Foreground := $00408080;
  fInterFuncAttri.Style := [fsBold];
  AddAttribute(fInterFuncAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := $00FF0080;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clTeal;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Style := [fsBold];
  AddAttribute(fSymbolAttri);

  fSysVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystemValue);
  fSysVarAttri.Foreground := $000080FF;
  fSysVarAttri.Style := [fsBold];
  AddAttribute(fSysVarAttri);

  SetAttributesOnChange(DefHighlightChange);

  AWKSyntaxList := TStringList.Create;
  MakeSyntaxList;

  MakeMethodTables;
  fDefaultFilter := SYNS_FilterAWK;
end;

destructor TSynAWKSyn.Destroy;
begin
  AWKSyntaxList.Free;

  inherited Destroy;
end;

procedure TSynAWKSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynAWKSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynAWKSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynAWKSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynAWKSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;

  while (fLine[Run] in [#1..#9, #11, #12, #14..#32]) do begin
    Inc(Run);
  end;
end;

function TSynAWKSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynAWKSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynAWKSyn.GetToken: string;
var
  len: Longint;
begin
  len := (Run - fTokenPos);
  SetString(Result, (fLine + fTokenPos), len);
end;

function TSynAWKSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynAWKSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkInterFunc: Result := fInterFuncAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSysVar: Result := fSysVarAttri;
  else
    Result := nil;
  end;
end;

function TSynAWKSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynAWKSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynAWKSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z'] + TSynSpecialChars;
end;

function TSynAWKSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterAWK;
end;

class function TSynAWKSyn.GetLanguageName: string;
begin
  Result := SYNS_LangAWK;
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynAWKSyn);
{$ENDIF}
end.
