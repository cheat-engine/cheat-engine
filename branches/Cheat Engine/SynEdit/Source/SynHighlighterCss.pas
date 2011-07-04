{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterEnhCSS.pas, released 2001-10-28
Initial modifications to this CSS Highlighter were made by Ashley Brown,
ashley@ashleybrown.co.uk.

The Original Code is based on the SynHighlighterHTML.pas, released 2000-04-10 - 
this in turn was based on the hkHTMLSyn.pas file from the mwEdit component suite
by Martin Waldenburg and other developers, the Initial Author of this file is
Hideo Koiso. All Rights Reserved.

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

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

You may retrieve the latest version of this file from
http://www.ashleybrown.co.uk/synedit/

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an improved CSS highlighter for SynEdit)
@author(Ashley Brown, based on HTML highlighter by Hideo Koiso and converted to SynEdit by Michael Hieke)
@created(2001-10-28)
@lastmod(2003-05-11)
The SynHighlighterEnhCSS unit provides SynEdit with an improved CSS highlighter.

http://www.ashleybrown.co.uk/
ashley@ashleybrown.co.uk
}

{$IFNDEF QSYNHIGHLIGHTERCSS}
unit SynHighlighterCSS;
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
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkProperty, tkKey, tkNull,
    tkSpace, tkString, tkSymbol, tkText, tkUndefProperty, tkValue, tkColor, tkNumber);

  TRangeState = ( rsComment, rsKey, rsParam, rsText,
    rsUnKnown, rsValue );

  TProcTableProc = procedure of object;

  TSynCssSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fCommentRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: Longint;
    Temp: PChar;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fPropertyAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fColorAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fUndefPropertyAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    fLineNumber: Integer;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;

    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure SemiProc;
    procedure StartValProc;
    procedure NumberProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure HashProc;
    procedure SlashProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property IdentChars;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property PropertyAttri: TSynHighlighterAttributes read fPropertyAttri
      write fPropertyAttri;
    property ColorAttri: TSynHighlighterAttributes read fColorAttri
      write fColorAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri
      write fValueAttri;
    property UndefPropertyAttri: TSynHighlighterAttributes read fUndefPropertyAttri
      write fUndefPropertyAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  mHashTable: array[#0..#255] of Integer;

const
  Properties: string =
               'azimuth,background,background-attachment,background-color,background-image,'+
               'background-position,background-repeat,border,border-collapse,border-color,'+
               'border-spacing,border-style,border-top border-right border-bottom border-left,'+
               'border-top-color border-right-color border-bottom-color border-left-color,'+
               'border-top-style border-right-style border-bottom-style border-left-style,'+
               'border-top-width border-right-width border-bottom-width border-left-width,'+
               'border-width,bottom,caption-side,clear,clip,color,content,counter-increment,'+
               'counter-reset,cue,cue-after,cue-before,cursor,direction,display,elevation,'+
               'empty-cells,float,font,font-family,font-size,font-size-adjust,font-stretch,'+
               'font-style,font-variant,font-weight,height,left,letter-spacing,line-height,'+
               'list-style,list-style-image,list-style-position,list-style-type,margin,'+
               'margin-top margin-right margin-bottom margin-left,marker-offset,marks,'+
               'max-height,max-width,min-height,min-width,orphans,outline,outline-color,'+
               'outline-style,outline-width,overflow,padding,padding-top,padding-right,'+
               'padding-bottom padding-left,page,page-break-after,page-break-before,'+
               'page-break-inside,pause,pause-after,pause-before,pitch,pitch-range,play-during,'+
               'position,quotes,richness,right,size,speak,speak-header,speak-numeral,'+
               'speak-punctuation,speech-rate,stress,table-layout,text-align,text-decoration,'+
               'text-indent,text-shadow,text-transform,top,unicode-bidi,vertical-align,'+
               'visibility,voice-family,volume,white-space,widows,width,word-spacing,z-index';

procedure MakeIdentTable;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      'a'..'z', 'A'..'Z':
        mHashTable[i] := (Ord(UpCase(i)) - 64);
      '!':
        mHashTable[i] := $7B;
      '/':
        mHashTable[i] := $7A;
      else
        mHashTable[Char(i)] := 0;
    end;
end;

{ TSynCssSyn }

function TSynCssSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  While (ToHash^ In ['a'..'z', 'A'..'Z', '-']) do begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  While (ToHash^ In ['0'..'9']) do begin
    Inc(Result, (Ord(ToHash^) - Ord('0')) );
    Inc(ToHash);
  end;
  fStringLen := (ToHash - fToIdent);
end;

function TSynCssSyn.KeyComp(const aKey: string): Boolean;
var
  i: Integer;
begin
  Temp := fToIdent;
  if (Length(aKey) = fStringLen) then begin
    Result := True;
    For i:=1 To fStringLen do begin
      if (mHashTable[Temp^] <> mHashTable[aKey[i]]) then begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end else begin
    Result := False;
  end;
end;

procedure TSynCssSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSynCssSyn.MakeMethodTables;
var
  i: Char;
begin
  For i:=#0 To #255 do begin
    case i of
    #0:
      fProcTable[i] := NullProc;
    #10:
      fProcTable[i] := LFProc;
    #13:
      fProcTable[i] := CRProc;
    #1..#9, #11, #12, #14..#32:
      fProcTable[i] := SpaceProc;
    '"':
      fProcTable[i] := StringProc;
    '#':
      fProcTable[i] := HashProc;
    '{':
      fProcTable[i] := BraceOpenProc;
    '}':
      fProcTable[i] := BraceCloseProc;
    ':', ',':
      fProcTable[i] := StartValProc;
    ';':
      fProcTable[i] := SemiProc;
    '0'..'9', '.':
      fProcTable[I] := NumberProc;
    '/':
      fProcTable[I] := SlashProc;
    else
      fProcTable[i] := IdentProc;
    end;
  end;
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  AddAttribute(fCommentAttri);

  fPropertyAttri := TSynHighlighterAttributes.Create('Property');
  fPropertyAttri.Style := [fsBold];
  AddAttribute(fPropertyAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := $00ff0080;
  AddAttribute(fKeyAttri);


  fUndefPropertyAttri := TSynHighlighterAttributes.Create('Undefined Property');
  fUndefPropertyAttri.Style := [fsBold];
  fUndefPropertyAttri.Foreground := $00ff0080;
  AddAttribute(fUndefPropertyAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fColorAttri := TSynHighlighterAttributes.Create(SYNS_AttrColor);
  AddAttribute(fColorAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(fTextAttri);

  fValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue);
  fValueAttri.Foreground := $00ff8000;
  AddAttribute(fValueAttri);

  SetAttributesOnChange(DefHighlightChange);

  MakeMethodTables;
  EnumerateKeywords(Ord(tkProperty), Properties, IdentChars, DoAddKeyword);

  fRange := rsText;
  fDefaultFilter := SYNS_FilterCSS;
end;

destructor TSynCssSyn.Destroy;
begin
  fKeywords.free;
  inherited Destroy;
end;

procedure TSynCssSyn.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCssSyn.BraceCloseProc;
begin
  fRange := rsText;
  fTokenId := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.CommentProc;
begin
  if fLine[Run] = #0 then
    fTokenID := tkNull
  else begin
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := fCommentRange;
        inc(Run, 2);
        break;
      end;
      inc(Run);
    until fLine[Run] = #0;
  end;
end;

procedure TSynCssSyn.BraceOpenProc;
begin
  Inc(Run);
  fRange := rsParam;
  fTokenID := tkSymbol;
end;

procedure TSynCssSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynCssSyn.SemiProc;
begin
  fRange := rsUnknown;
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.StartValProc;
begin
  fRange := rsValue;
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynCssSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

function TSynCssSyn.IdentKind(MayBe: PChar): TtkTokenKind;
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
  Result := tkUndefProperty;
end;

procedure TSynCssSyn.IdentProc;
begin
  case fRange of
    rsKey:
      begin
        fRange := rsParam;
        fTokenID := tkKey;
        Inc(Run, fStringLen);
      end;
    rsValue:
      begin
        fRange := rsParam;
        fTokenID := tkValue;

        while not (fLine[Run] In [#0, #10, #13,  '}', ';', ',']) do
          Inc(Run);
      end;
    else
      fTokenID := IdentKind((fLine + Run));
      repeat
        Inc(Run);
      until (fLine[Run] In [#0..#32, ':', '"', '}', ';']);
  end;
end;

procedure TSynCssSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCssSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynCssSyn.TextProc;
const StopSet = [#0..#31, '{', '/'];
begin
  if fLine[Run] in (StopSet) then
  begin
    fProcTable[fLine[Run]];
    exit;
  end;

  fTokenID := tkKey;
  while not (fLine[Run] in StopSet) do Inc(Run);
end;

procedure TSynCssSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] <= #32 do
  begin
    if fLine[Run] in [#0, #9, #10, #13] then break;
    Inc(Run);
  end;
end;

procedure TSynCssSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);  // first '"'
  while not (fLine[Run] in [#0, #10, #13, '"']) do Inc(Run);
  if fLine[Run] = '"' then Inc(Run);  // last '"'
end;

procedure TSynCssSyn.HashProc;
begin
  fTokenID := tkColor;
  Inc(Run);  // '#'
  while (fLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(Run);
end;


procedure TSynCssSyn.SlashProc;
begin
  inc(Run);
  if fLine[Run] = '*' then
  begin
    fTokenID := tkComment;
    fCommentRange := fRange;
    fRange := rsComment;
    inc(Run);
    if not (fLine[Run] in [#0, #10, #13]) then
      CommentProc;
  end
  else
    fTokenID := tkSymbol;
end;


procedure TSynCssSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsText:
      TextProc;
    rsComment:
      CommentProc;
    else
      fProcTable[fLine[Run]];
  end;
end;

function TSynCssSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynCssSyn.GetToken: string;
var
  len: Longint;
begin
  Len := (Run - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

function TSynCssSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCssSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkProperty: Result := fPropertyAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkText: Result := fTextAttri;
    tkUndefProperty: Result := fUndefPropertyAttri;
    tkValue: Result := fValueAttri;
    tkColor: Result := fColorAttri;
    tkNumber: Result := fNumberAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCssSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynCssSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynCssSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynCssSyn.ResetRange;
begin
  fRange:= rsText;
end;

function TSynCssSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-'];
end;

function TSynCssSyn.GetSampleSource: string;
begin
  Result := '/* Syntax Highlighting */'#13#10 +
        'body { font-family: Tahoma, Verdana, Arial, Helvetica, sans-serif; font-size: 8pt }'#13#10 +
        'H1 { font-size: 18pt; color: #000099; made-up-property: 1 }';
end; { GetSampleSource }

class function TSynCssSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCSS;
end;

function TSynCssSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCSS;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCssSyn);
{$ENDIF}
end.
