{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterTex.pas, released 2002-09-18.
Author of this file is Soeren Sproessig.
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

$Id: SynHighlighterTeX.pas,v 1.5 2004/07/13 00:00:32 markonjezic Exp $

You may retrieve the latest version of this file from sproessig@bs-webdesign.de

The unit SynHighlighterTeX provides SynEdit with a TeX highlighter.

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERTEX}
unit SynHighlighterTeX;
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
  Classes;

type
  TtkTokenKind = (tkBrace, tkBracket, tkNull, tkSpace, tkText, tkComment,
                  tkControlSequence, tkMathMode);

  TProcTableProc = procedure of object;

type
  TSynTeXSyn = class(TSynCustomHighlighter)
  private
    fLine:                  PChar;
    fLineNumber:            Integer;
    fProcTable:             array[#0..#255] of TProcTableProc;
    Run:                    LongInt;
    fTokenPos:              Integer;
    fTokenID:               TtkTokenKind;
    fTextAttri:             TSynHighlighterAttributes;
    fControlSequenceAttri:  TSynHighlighterAttributes;
    fMathmodeAttri:         TSynHighlighterAttributes;
    fCommentAttri:          TSynHighlighterAttributes;
    fSpaceAttri:            TSynHighlighterAttributes;
    fBracketAttri:          TSynHighlighterAttributes;
    fBraceAttri:            TSynHighlighterAttributes;

    function CreateHighlighterAttributes(Name:String; Foreground,
                                         Background: TColor;
                                         FontStyles: TFontStyles) :
                                         TSynHighlighterAttributes;
    //Procedures
    procedure MakeMethodTables;
    procedure CRProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure ControlSequenceProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure BracketOpenProc;
    procedure BracketCloseProc;
    procedure MathmodeProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource : String; override;
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
    property CommentAttri : TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property ControlSequenceAttri: TSynHighlighterAttributes read
fControlSequenceAttri
      write fControlSequenceAttri;
    property MathmodeAttri: TSynHighlighterAttributes read fMathmodeAttri
      write fMathmodeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property BraceAttri: TSynHighlighterAttributes read fBraceAttri
      write fBraceAttri;
    property BracketAttri: TSynHighlighterAttributes read fBracketAttri
      write fBracketAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

procedure TSynTeXSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0                         : fProcTable[i] := NullProc;
      #10                        : fProcTable[i] := LFProc;
      #13                        : fProcTable[i] := CRProc;
      #37                        : fProcTable[i] := CommentProc;
      #92                        : fProcTable[i] := ControlSequenceProc;
      #123                       : fProcTable[i] := BraceOpenProc;
      #125                       : fProcTable[i] := BraceCloseProc;
      #91                        : fProcTable[i] := BracketOpenProc;
      #93                        : fProcTable[i] := BracketCloseProc;
      #1..#9, #11, #12, #14..#32 : fProcTable[i] := SpaceProc;
      #36                        : fProcTable[i] := MathmodeProc;
    else
      fProcTable[i] := TextProc;
    end;
end;

constructor TSynTeXSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCommentAttri:=CreateHighlighterAttributes(SYNS_AttrComment,clTeal,clNone,[]);
  AddAttribute(fCommentAttri);

  fTextAttri:= CreateHighlighterAttributes(SYNS_AttrText,clBlack,clNone,[]);
  AddAttribute(fTextAttri);

  fMathmodeAttri:=CreateHighlighterAttributes(SYNS_AttrMathmode,clOlive,clNone,[fsbold]);
  AddAttribute(fMathmodeAttri);

  fSpaceAttri:=CreateHighlighterAttributes(SYNS_AttrSpace,clNone,clWhite,[]);
  AddAttribute(fSpaceAttri);

  fControlSequenceAttri:=CreateHighlighterAttributes(SYNS_AttrTexCommand,clBlue,clWhite,[fsBold]);
  AddAttribute(fControlSequenceAttri);

  fBracketAttri:=CreateHighlighterAttributes(SYNS_AttrSquareBracket,clPurple,clNone,[]);
  AddAttribute(fBracketAttri);

  fBraceAttri:=
  CreateHighlighterAttributes(SYNS_AttrRoundBracket,clRed,clNone,[fsBold]);
  AddAttribute(fBraceAttri);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterTeX;
  MakeMethodTables;
end;  { Create }

procedure TSynTeXSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine       := PChar(NewValue);
  Run         := 0;
  fLineNumber := LineNumber;
  Next;
end;  { SetLine }

procedure TSynTeXSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;  { CRProc }


procedure TSynTeXSyn.SpaceProc;
begin
  fTokenID:=tkSpace;
  inc(Run);
   while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;  { SpaceProc }

procedure TSynTeXSyn.TextProc;
begin
  fTokenID:=tkText;
  inc(Run);
end;  { TextProc }

procedure TSynTeXSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;  { SpaceProc }

procedure TSynTeXSyn.BraceOpenProc;
begin
  fTokenID := tkBrace;
  inc(Run);
end;  { BraceOpen }

procedure TSynTeXSyn.BraceCloseProc;
begin
  fTokenID := tkBrace;
  inc(Run);
end;  { BraceClose }

procedure TSynTeXSyn.BracketOpenProc;
begin
  fTokenID := tkBracket;
  inc(Run);
end;  { BracketOpen }

procedure TSynTeXSyn.BracketCloseProc;
begin
  fTokenID := tkBracket;
  inc(Run);
end;  { BracketClose }

procedure TSynTeXSyn.NullProc;
begin
  fTokenID := tkNull;
end;  { NullProc }

procedure TSynTeXSyn.CommentProc;
begin
 fTokenID := tkComment;
 repeat
    case fLine[Run] of
      #0, #10: Break;
    end;
    inc(Run);
  until fLine[Run] = #13;
  Exit;
end;  { CommentProc }

procedure TSynTeXSyn.MathModeProc;
begin
 fTokenID:=tkMathMode;
 Inc(Run);
end;  { MathModeProc }

procedure TSynTeXSyn.ControlSequenceProc;
begin
 fTokenID:=tkControlSequence;
 repeat
   case fLine[Run] of
     #0..#31                 : Break;  //No Control Chars !
     #48..#57                : Break;  //No Numbers !
     #33..#47, #58..#64,               //Just the Characters that
     #91, #93,#94, #123,              //only can follow to '\'
     #125, #126              : begin
                                if (fLine[Run-1]='\') then
                                Inc(Run,1);
                                Break;
                               end;
   end;
   Inc(Run);
 until fLine[Run] = #32;
 exit;
end;  { ControlSequenceProc }

procedure TSynTeXSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;  { Next }

function TSynTeXSyn.GetDefaultAttribute(Index: integer):
TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynTeXSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;  { GetDefaultAttribute }

function TSynTeXSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;  { GetToken }

function TSynTeXSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;  { GetTokenID }

function TSynTeXSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment                      : Result := fCommentAttri;
    tkText                         : Result := fTextAttri;
    tkControlSequence              : Result := fControlSequenceAttri;
    tkMathMode                     : Result := fMathmodeAttri;
    tkSpace                        : Result := fSpaceAttri;
    tkBrace                        : Result := fBraceAttri;
    tkBracket                      : Result := fBracketAttri;
  else
    Result := nil;
  end;
end;  { GetTokenAttribute }

function TSynTeXSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;  { GetTokenKind }

function TSynTeXSyn.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;  { GetTokenPos }

function TSynTeXSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;  { GetIdentChars }

function TSynTeXSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTeX;
end;

class function TSynTeXSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTeX;
end;  { GetLanguageName }

function TSynTeXSyn.CreateHighlighterAttributes(Name:String; Foreground,
                                                Background: TColor;
                                                FontStyles: TFontStyles) :
TSynHighlighterAttributes;
begin
  Result:=TSynHighlighterAttributes.Create(Name);
  if Foreground<>clNone then Result.Foreground:=ForeGround;
  if Background<>clNone then Result.Background:=Background;
  Result.Style:=FontStyles;
end;

function TSynTeXSyn.GetSampleSource: String;
begin
  Result:='\documentclass[a4paper]{article}'+#13#10+
          '% LaTeX sample source'+#13#10+
          '\begin{document}'+#13#10+
          'Here is a formula: $ (2x + 3)*5y $'+#13#10+
          '\end{document}';
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynTeXSyn);
{$ENDIF}
end.
