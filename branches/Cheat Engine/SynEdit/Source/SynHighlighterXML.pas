{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterXML.pas, released 2000-11-20.
The Initial Author of this file is Jeff Rafter.
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

$Id: SynHighlighterXML.pas,v 1.11 2004/07/15 10:49:34 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

History:
-------------------------------------------------------------------------------
2000-11-30 Removed mHashTable and MakeIdentTable per Michael Hieke

Known Issues:
- Nothing is really constrained (properly) to valid name chars
- Entity Refs are not constrained to valid name chars
- Support for "Combining Chars and Extender Chars" in names are lacking
- The internal DTD is not parsed (and not handled correctly)
-------------------------------------------------------------------------------}
{
@abstract(Provides an XML highlighter for SynEdit)
@author(Jeff Rafter-- Phil 4:13, based on SynHighlighterHTML by Hideo Koiso)
@created(2000-11-17)
@lastmod(2001-03-12)
The SynHighlighterXML unit provides SynEdit with an XML highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERXML}
unit SynHighlighterXML;
{$ENDIF}

interface

{$I SynEdit.inc}

uses
{$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAposAttrValue, tkAposEntityRef, tkAttribute, tkCDATA,
    tkComment, tkElement, tkEntityRef, tkEqual, tkNull, tkProcessingInstruction,
    tkQuoteAttrValue, tkQuoteEntityRef, tkSpace, tkSymbol, tkText,
    //
    tknsAposAttrValue, tknsAposEntityRef, tknsAttribute, tknsEqual,
    tknsQuoteAttrValue, tknsQuoteEntityRef,
    //These are unused at the moment
    tkDocType
    {tkDocTypeAposAttrValue, tkDocTypeAposEntityRef, tkDocTypeAttribute,
     tkDocTypeElement, tkDocTypeEqual tkDocTypeQuoteAttrValue,
     tkDocTypeQuoteEntityRef}
  );

  TRangeState = (rsAposAttrValue, rsAPosEntityRef, rsAttribute, rsCDATA,
    rsComment, rsElement, rsEntityRef, rsEqual, rsProcessingInstruction,
    rsQuoteAttrValue, rsQuoteEntityRef, rsText,
    //
    rsnsAposAttrValue, rsnsAPosEntityRef, rsnsEqual, rsnsQuoteAttrValue,
    rsnsQuoteEntityRef,
    //These are unused at the moment
    rsDocType, rsDocTypeSquareBraces
    {rsDocTypeAposAttrValue, rsDocTypeAposEntityRef, rsDocTypeAttribute,
     rsDocTypeElement, rsDocTypeEqual, rsDocTypeQuoteAttrValue,
     rsDocTypeQuoteEntityRef}
  );

  TProcTableProc = procedure of object;

  TSynXMLSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    Run: Longint;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fLineNumber: Integer;
    fElementAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fEntityRefAttri: TSynHighlighterAttributes;
    fProcessingInstructionAttri: TSynHighlighterAttributes;
    fCDATAAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDocTypeAttri: TSynHighlighterAttributes;
    fAttributeAttri: TSynHighlighterAttributes;
    fnsAttributeAttri: TSynHighlighterAttributes;
    fAttributeValueAttri: TSynHighlighterAttributes;
    fnsAttributeValueAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fProcTable: array[#0..#255] of TProcTableProc;
    FWantBracesParsed: Boolean;
    procedure NullProc;
    procedure CarriageReturnProc;
    procedure LineFeedProc;
    procedure SpaceProc;
    procedure LessThanProc;
    procedure GreaterThanProc;
    procedure CommentProc;
    procedure ProcessingInstructionProc;
    procedure DocTypeProc;
    procedure CDATAProc;
    procedure TextProc;
    procedure ElementProc;
    procedure AttributeProc;
    procedure QAttributeValueProc;
    procedure AAttributeValueProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure MakeMethodTables;
    function NextTokenIs(T: String): Boolean;
    procedure EntityRefProc;
    procedure QEntityRefProc;
    procedure AEntityRefProc;
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
    property ElementAttri: TSynHighlighterAttributes read fElementAttri
      write fElementAttri;
    property AttributeAttri: TSynHighlighterAttributes read fAttributeAttri
      write fAttributeAttri;
    property NamespaceAttributeAttri: TSynHighlighterAttributes
      read fnsAttributeAttri write fnsAttributeAttri;
    property AttributeValueAttri: TSynHighlighterAttributes
      read fAttributeValueAttri write fAttributeValueAttri;
    property NamespaceAttributeValueAttri: TSynHighlighterAttributes
      read fnsAttributeValueAttri write fnsAttributeValueAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property CDATAAttri: TSynHighlighterAttributes read fCDATAAttri
      write fCDATAAttri;
    property EntityRefAttri: TSynHighlighterAttributes read fEntityRefAttri
      write fEntityRefAttri;
    property ProcessingInstructionAttri: TSynHighlighterAttributes
      read fProcessingInstructionAttri write fProcessingInstructionAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DocTypeAttri: TSynHighlighterAttributes read fDocTypeAttri
      write fDocTypeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property WantBracesParsed : Boolean read FWantBracesParsed
      write FWantBracesParsed default True;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  NameChars : set of char = ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', ':', '-'];

constructor TSynXMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fElementAttri:= TSynHighlighterAttributes.Create(SYNS_AttrElementName);
  fTextAttri:= TSynHighlighterAttributes.Create(SYNS_AttrText);
  fSpaceAttri:= TSynHighlighterAttributes.Create(SYNS_AttrWhitespace);
  fEntityRefAttri:= TSynHighlighterAttributes.Create(SYNS_AttrEntityReference);
  fProcessingInstructionAttri:= TSynHighlighterAttributes.Create(
    SYNS_AttrProcessingInstr);
  fCDATAAttri:= TSynHighlighterAttributes.Create(SYNS_AttrCDATASection);
  fCommentAttri:= TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fDocTypeAttri:= TSynHighlighterAttributes.Create(SYNS_AttrDOCTYPESection);
  fAttributeAttri:= TSynHighlighterAttributes.Create(SYNS_AttrAttributeName);
  fnsAttributeAttri:= TSynHighlighterAttributes.Create(
    SYNS_AttrNamespaceAttrName);
  fAttributeValueAttri:= TSynHighlighterAttributes.Create(
    SYNS_AttrAttributeValue);
  fnsAttributeValueAttri:= TSynHighlighterAttributes.Create(
    SYNS_AttrNamespaceAttrValue);
  fSymbolAttri:= TSynHighlighterAttributes.Create(SYNS_AttrSymbol);

  fElementAttri.Foreground:= clMaroon;
  fElementAttri.Style:= [fsBold];

  fDocTypeAttri.Foreground:= clblue;
  fDocTypeAttri.Style:= [fsItalic];

  fCDATAAttri.Foreground:= clOlive;
  fCDATAAttri.Style:= [fsItalic];

  fEntityRefAttri.Foreground:= clblue;
  fEntityRefAttri.Style:= [fsbold];

  fProcessingInstructionAttri.Foreground:= clblue;
  fProcessingInstructionAttri.Style:= [];

  fTextAttri.Foreground:= clBlack;
  fTextAttri.Style:= [fsBold];

  fAttributeAttri.Foreground:= clMaroon;
  fAttributeAttri.Style:= [];

  fnsAttributeAttri.Foreground:= clRed;
  fnsAttributeAttri.Style:= [];

  fAttributeValueAttri.Foreground:= clNavy;
  fAttributeValueAttri.Style:= [fsBold];

  fnsAttributeValueAttri.Foreground:= clRed;
  fnsAttributeValueAttri.Style:= [fsBold];

  fCommentAttri.Background:= clSilver;
  fCommentAttri.Foreground:= clGray;
  fCommentAttri.Style:= [fsbold, fsItalic];

  fSymbolAttri.Foreground:= clblue;
  fSymbolAttri.Style:= [];

  AddAttribute(fSymbolAttri);
  AddAttribute(fProcessingInstructionAttri);
  AddAttribute(fDocTypeAttri);
  AddAttribute(fCommentAttri);
  AddAttribute(fElementAttri);
  AddAttribute(fAttributeAttri);
  AddAttribute(fnsAttributeAttri);
  AddAttribute(fAttributeValueAttri);
  AddAttribute(fnsAttributeValueAttri);
  AddAttribute(fEntityRefAttri);
  AddAttribute(fCDATAAttri);
  AddAttribute(fSpaceAttri);
  AddAttribute(fTextAttri);

  SetAttributesOnChange(DefHighlightChange);

  MakeMethodTables;
  fRange := rsText;
  fDefaultFilter := SYNS_FilterXML;
end;

procedure TSynXMLSyn.MakeMethodTables;
var
  i: Char;
begin
  for i:= #0 To #255 do begin
    case i of
    #0:
      begin
        fProcTable[i] := NullProc;
      end;
    #10:
      begin
        fProcTable[i] := LineFeedProc;
      end;
    #13:
      begin
        fProcTable[i] := CarriageReturnProc;
      end;
    #1..#9, #11, #12, #14..#32:
      begin
        fProcTable[i] := SpaceProc;
      end;
    '<':
      begin
        fProcTable[i] := LessThanProc;
      end;
    '>':
      begin
        fProcTable[i] := GreaterThanProc;
      end;
    else
      fProcTable[i] := IdentProc;
    end;
  end;
end;

procedure TSynXMLSyn.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynXMLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynXMLSyn.CarriageReturnProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynXMLSyn.LineFeedProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynXMLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] <= #32 do begin
    if fLine[Run] in [#0, #9, #10, #13] then break;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.LessThanProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') then
    Inc(Run);

  if (fLine[Run] = '!') then
  begin
    if NextTokenIs('--') then begin
      fTokenID := tkSymbol;
      fRange := rsComment;
      Inc(Run, 3);
    end else if NextTokenIs('DOCTYPE') then begin
      fTokenID := tkDocType;
      fRange := rsDocType;
      Inc(Run, 7);
    end else if NextTokenIs('[CDATA[') then begin
      fTokenID := tkCDATA;
      fRange := rsCDATA;
      Inc(Run, 7);
    end else begin
      fTokenID := tkSymbol;
      fRange := rsElement;
      Inc(Run);
    end;
  end else if fLine[Run]= '?' then begin
    fTokenID := tkProcessingInstruction;
    fRange := rsProcessingInstruction;
    Inc(Run);
  end else begin
    fTokenID := tkSymbol;
    fRange := rsElement;
  end;
end;

procedure TSynXMLSyn.GreaterThanProc;
begin
  fTokenId := tkSymbol;
  fRange:= rsText;
  Inc(Run);
end;

procedure TSynXMLSyn.CommentProc;
begin
  if (fLine[Run] = '-') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '>')
  then begin
    fTokenID := tkSymbol;
    fRange:= rsText;
    Inc(Run, 3);
    Exit;
  end;

  fTokenID := tkComment;

  if (fLine[Run] In [#0, #10, #13]) then begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do begin
    if (fLine[Run] = '-') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '>')
    then begin
      fRange := rsComment;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.ProcessingInstructionProc;
begin
  fTokenID := tkProcessingInstruction;
  if (fLine[Run] In [#0, #10, #13]) then begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '?')
    then begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.DocTypeProc;
begin
  fTokenID := tkDocType;

  if (fLine[Run] In [#0, #10, #13]) then begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  case fRange of
    rsDocType:
      begin
        while not (fLine[Run] in [#0, #10, #13]) do
        begin
          case fLine[Run] of
            '[': begin
                   while True do
                   begin
                     inc(Run);
                     case fLine[Run] of
                       ']':
                         begin
                           Inc(Run);
                           Exit;
                         end;
                       #0, #10, #13:
                         begin
                           fRange:=rsDocTypeSquareBraces;
                           Exit;
                         end;
                     end;
                   end;
                 end;
            '>': begin
                   fRange := rsAttribute;
                   Inc(Run);
                   Break;
                 end;
          end;
          inc(Run);
        end;
    end;
    rsDocTypeSquareBraces:
      begin
        while not (fLine[Run] in [#0, #10, #13]) do
        begin
          if (fLine[Run]=']') then
          begin
            fRange := rsDocType;
            Inc(Run);
            Exit;
          end;
          inc(Run);
        end;
      end;
  end;
end;

procedure TSynXMLSyn.CDATAProc;
begin
  fTokenID := tkCDATA;
  if (fLine[Run] In [#0, #10, #13]) then
  begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = ']')
    then begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.ElementProc;
begin
  if fLine[Run] = '/' then Inc(Run);
{$IFDEF SYN_MBCSSUPPORT}
  while (fLine[Run] in NameChars) or (StrByteType(fLine, Run) <> mbSingleByte) do
    Inc(Run);
{$ELSE}
  while (fLine[Run] in NameChars) do Inc(Run);
{$ENDIF}
  fRange := rsAttribute;
  fTokenID := tkElement;
end;

procedure TSynXMLSyn.AttributeProc;
begin
  //Check if we are starting on a closing quote
  if (fLine[Run] in [#34, #39]) then
  begin
    fTokenID := tkSymbol;
    fRange := rsAttribute;
    Inc(Run);
    Exit;
  end;
  //Read the name
{$IFDEF SYN_MBCSSUPPORT}
  while (fLine[Run] in NameChars) or (StrByteType(fLine, Run) <> mbSingleByte) do
    Inc(Run);
{$ELSE}
  while (fLine[Run] in NameChars) do Inc(Run);
{$ENDIF}
  //Check if this is an xmlns: attribute
  if (Pos('xmlns', GetToken) > 0) then begin
    fTokenID := tknsAttribute;
    fRange := rsnsEqual;
  end else begin
    fTokenID := tkAttribute;
    fRange := rsEqual;
  end;
end;

procedure TSynXMLSyn.EqualProc;
begin
  if fRange = rsnsEqual then
    fTokenID := tknsEqual
  else
    fTokenID := tkEqual;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '/') then
    begin
      fTokenID := tkSymbol;
      fRange := rsElement;
      Inc(Run);
      Exit;
    end else if (fLine[Run] = #34) then
    begin
      if fRange = rsnsEqual then
        fRange := rsnsQuoteAttrValue
      else
        fRange := rsQuoteAttrValue;
      Inc(Run);
      Exit;
    end else if (fLine[Run] = #39) then
    begin
      if fRange = rsnsEqual then
        fRange := rsnsAPosAttrValue
      else
        fRange := rsAPosAttrValue;
      Inc(Run);
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.QAttributeValueProc;
begin
  if fRange = rsnsQuoteAttrValue then
    fTokenID := tknsQuoteAttrValue
  else
    fTokenID := tkQuoteAttrValue;

  while not (fLine[Run] in [#0, #10, #13, '&', #34]) do Inc(Run);

  if fLine[Run] = '&' then
  begin
    if fRange = rsnsQuoteAttrValue then
      fRange := rsnsQuoteEntityRef
    else
      fRange := rsQuoteEntityRef;
    Exit;
  end else if fLine[Run] <> #34 then
  begin
    Exit;
  end;

  fRange := rsAttribute;
end;

procedure TSynXMLSyn.AAttributeValueProc;
begin
  if fRange = rsnsAPosAttrValue then
    fTokenID := tknsAPosAttrValue
  else
    fTokenID := tkAPosAttrValue;

  while not (fLine[Run] in [#0, #10, #13, '&', #39]) do Inc(Run);

  if fLine[Run] = '&' then
  begin
    if fRange = rsnsAPosAttrValue then
      fRange := rsnsAPosEntityRef
    else
      fRange := rsAPosEntityRef;
    Exit;
  end else if fLine[Run] <> #39 then
  begin
    Exit;
  end;

  fRange := rsAttribute;
end;

procedure TSynXMLSyn.TextProc;
const StopSet = [#0..#31, '<', '&'];
begin
  if fLine[Run] in (StopSet - ['&']) then begin
    fProcTable[fLine[Run]];
    exit;
  end;

  fTokenID := tkText;
  while not (fLine[Run] in StopSet) do Inc(Run);

  if (fLine[Run] = '&') then begin
    fRange := rsEntityRef;
    Exit;
  end;
end;

procedure TSynXMLSyn.EntityRefProc;
begin
  fTokenID := tkEntityRef;
  fRange := rsEntityRef;
  while not (fLine[Run] in [#0..#32, ';']) do Inc(Run);
  if (fLine[Run] = ';') then Inc(Run);
  fRange := rsText;
end;

procedure TSynXMLSyn.QEntityRefProc;
begin
  if fRange = rsnsQuoteEntityRef then
    fTokenID := tknsQuoteEntityRef
  else
    fTokenID := tkQuoteEntityRef;

  while not (fLine[Run] in [#0..#32, ';']) do Inc(Run);
  if (fLine[Run] = ';') then Inc(Run);

  if fRange = rsnsQuoteEntityRef then
    fRange := rsnsQuoteAttrValue
  else
    fRange := rsQuoteAttrValue;
end;

procedure TSynXMLSyn.AEntityRefProc;
begin
  if fRange = rsnsAPosEntityRef then
    fTokenID := tknsAPosEntityRef
  else
    fTokenID := tkAPosEntityRef;

  while not (fLine[Run] in [#0..#32, ';']) do Inc(Run);
  if (fLine[Run] = ';') then Inc(Run);

  if fRange = rsnsAPosEntityRef then
    fRange := rsnsAPosAttrValue
  else
    fRange := rsAPosAttrValue;
end;

procedure TSynXMLSyn.IdentProc;
begin
  case fRange of
  rsElement:
    begin
      ElementProc;
    end;
  rsAttribute:
    begin
      AttributeProc;
    end;
  rsEqual, rsnsEqual:
    begin
      EqualProc;
    end;
  rsQuoteAttrValue, rsnsQuoteAttrValue:
    begin
      QAttributeValueProc;
    end;
  rsAposAttrValue, rsnsAPosAttrValue:
    begin
      AAttributeValueProc;
    end;
  rsQuoteEntityRef, rsnsQuoteEntityRef:
    begin
      QEntityRefProc;
    end;
  rsAposEntityRef, rsnsAPosEntityRef:
    begin
      AEntityRefProc;
    end;
  rsEntityRef:
    begin
      EntityRefProc;
    end;
  else ;
  end;
end;

procedure TSynXMLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
  rsText:
    begin
      TextProc;
    end;
  rsComment:
    begin
      CommentProc;
    end;
  rsProcessingInstruction:
    begin
      ProcessingInstructionProc;
    end;
  rsDocType, rsDocTypeSquareBraces:
    begin
      DocTypeProc;
    end;
  rsCDATA:
    begin
      CDATAProc;
    end;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynXMLSyn.NextTokenIs(T : String) : Boolean;
var I, Len : Integer;
begin
  Result:= True;
  Len:= Length(T);
  for I:= 1 to Len do
    if (fLine[Run + I] <> T[I]) then
    begin
      Result:= False;
      Break;
    end;
end;

function TSynXMLSyn.GetDefaultAttribute(
  Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fAttributeAttri;
    SYN_ATTR_KEYWORD: Result := fElementAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynXMLSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynXMLSyn.GetToken: string;
var
  len: Longint;
begin
  Len := (Run - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

function TSynXMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynXMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkElement: Result:= fElementAttri;
    tkAttribute: Result:= fAttributeAttri;
    tknsAttribute: Result:= fnsAttributeAttri;
    tkEqual: Result:= fSymbolAttri;
    tknsEqual: Result:= fSymbolAttri;
    tkQuoteAttrValue: Result:= fAttributeValueAttri;
    tkAPosAttrValue: Result:= fAttributeValueAttri;
    tknsQuoteAttrValue: Result:= fnsAttributeValueAttri;
    tknsAPosAttrValue: Result:= fnsAttributeValueAttri;
    tkText: Result:= fTextAttri;
    tkCDATA: Result:= fCDATAAttri;
    tkEntityRef: Result:= fEntityRefAttri;
    tkQuoteEntityRef: Result:= fEntityRefAttri;
    tkAposEntityRef: Result:= fEntityRefAttri;
    tknsQuoteEntityRef: Result:= fEntityRefAttri;
    tknsAposEntityRef: Result:= fEntityRefAttri;
    tkProcessingInstruction: Result:= fProcessingInstructionAttri;
    tkComment: Result:= fCommentAttri;
    tkDocType: Result:= fDocTypeAttri;
    tkSymbol: Result:= fSymbolAttri;
    tkSpace: Result:= fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynXMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynXMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynXMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynXMLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynXMLSyn.ResetRange;
begin
  fRange:= rsText;
end;

function TSynXMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', '-'] + TSynSpecialChars;
end;

function TSynXMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterXML;
end;

class function TSynXMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangXML;
end;

function TSynXMLSyn.GetSampleSource: String;
begin
  Result:= '<?xml version="1.0"?>'#13#10+
           '<!DOCTYPE root ['#13#10+
           '  ]>'#13#10+
           '<!-- Comment -->'#13#10+
           '<root version="&test;">'#13#10+
           '  <![CDATA[ **CDATA section** ]]>'#13#10+
           '</root>';
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynXMLSyn);
{$ENDIF}
end.
