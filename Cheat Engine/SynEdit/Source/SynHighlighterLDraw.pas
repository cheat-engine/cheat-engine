{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterLDraw.pas, released 2003-04-12.
Description: LDraw Parser/Highlighter
The initial author of this file is Orion Pobursky.
Copyright (c) 2003, all rights reserved.

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

$Id: SynHighlighterLDraw.pas,v 1.8 2005/01/28 16:53:24 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an LDraw syntax highlighter for SynEdit)
@author(Orion Pobursky)
@created(03/01/2003)
@lastmod(07/05/2003)
The SynHighlighterLDraw unit provides SynEdit with a LEGO LDraw (.ldr / .dat) highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERLDRAW}
unit SynHighlighterLDraw;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
{$ELSE}
  Windows, Controls, Graphics,
  SynEditHighlighter, SynEditTypes,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkColor,
    tkComment,
    tkFirstTri,
    tkFourthTri,
    tkIdentifier,
    tkKey,
    tkLine,
    tkNull,
    tkOpLine,
    tkQuad,
    tkSecondTri,
    tkThirdTri,
    tkTriangle,
    tkUnknown);

  TRangeState = (rsUnKnown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 83;

type
  TSynLDRSyn = class(TSynCustomHighlighter)
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
    fColorAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fFirstTriAttri: TSynHighlighterAttributes;
    fFourthTriAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fLineAttri: TSynHighlighterAttributes;
    fOpLineAttri: TSynHighlighterAttributes;
    fQuadAttri: TSynHighlighterAttributes;
    fSecondTriAttri: TSynHighlighterAttributes;
    fThirdTriAttri: TSynHighlighterAttributes;
    fTriangleAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func83: TtkTokenKind;
    procedure IdentProc;
    procedure Number1Proc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure CRProc;
    procedure LFProc;
    function FirstChar(DatLine: PChar): Char;
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
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property ColorAttri: TSynHighlighterAttributes read fColorAttri write fColorAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property FirstTriAttri: TSynHighlighterAttributes read fFirstTriAttri write fFirstTriAttri;
    property FourthTriAttri: TSynHighlighterAttributes read fFourthTriAttri write fFourthTriAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property LineAttri: TSynHighlighterAttributes read fLineAttri write fLineAttri;
    property OpLineAttri: TSynHighlighterAttributes read fOpLineAttri write fOpLineAttri;
    property QuadAttri: TSynHighlighterAttributes read fQuadAttri write fQuadAttri;
    property SecondTriAttri: TSynHighlighterAttributes read fSecondTriAttri write fSecondTriAttri;
    property ThirdTriAttri: TSynHighlighterAttributes read fThirdTriAttri write fThirdTriAttri;
    property TriangleAttri: TSynHighlighterAttributes read fTriangleAttri write fTriangleAttri;
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

{$IFDEF SYN_CLX}
function RGB(CONST r, g, b:  BYTE):  TColor;
begin
  RESULT := (r OR (g SHL 8) OR (b SHL 16))
end;
{$ENDIF}

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynLDRSyn.InitIdent;
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
  fIdentFuncTable[83] := Func83;
end;

function TSynLDRSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynLDRSyn.KeyComp(const aKey: String): Boolean;
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
  end
  else
    Result := False;
end;

function TSynLDRSyn.Func83: TtkTokenKind;
begin
  if KeyComp('Author') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLDRSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynLDRSyn.IdentKind(MayBe: PChar): TtkTokenKind;
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

procedure TSynLDRSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      '0'..'9': fProcTable[I] := Number1Proc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynLDRSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynLDRSyn.CRProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynLDRSyn.LFProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

constructor TSynLDRSyn.Create(AOwner: TComponent);

  {$IFDEF SYN_KYLIX}
  function RGB(r, g, b: Byte): LongWord;
  begin
    Result := (r or (g shl 8) or (b shl 16));
  end;
  {$ENDIF}

begin
  inherited Create(AOwner);
  fColorAttri := TSynHighLighterAttributes.Create(SYNS_AttrColor);
  fColorAttri.Foreground := clNavy;
  AddAttribute(fColorAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clBlue;
  AddAttribute(fCommentAttri);

  fFirstTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrFirstTri);
  fFirstTriAttri.Foreground := RGB(206,111,73);
  AddAttribute(fFirstTriAttri);

  fFourthTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrFourthTri);
  fFourthTriAttri.Foreground := RGB(54,99,12);
  AddAttribute(fFourthTriAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fLineAttri := TSynHighLighterAttributes.Create(SYNS_AttrLine);
  fLineAttri.Foreground := clBlack;
  AddAttribute(fLineAttri);

  fOpLineAttri := TSynHighLighterAttributes.Create(SYNS_AttrOpLine);
  fOpLineAttri.Foreground := clBlack;
  AddAttribute(fOpLineAttri);

  fQuadAttri := TSynHighLighterAttributes.Create(SYNS_AttrQuad);
  fQuadAttri.Foreground := clRed;
  AddAttribute(fQuadAttri);

  fSecondTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrSecondTri);
  fSecondTriAttri.Foreground := RGB(54,99,12);
  AddAttribute(fSecondTriAttri);

  fThirdTriAttri := TSynHighLighterAttributes.Create(SYNS_AttrThirdTri);
  fThirdTriAttri.Foreground := RGB(206,111,73);
  AddAttribute(fThirdTriAttri);

  fTriangleAttri := TSynHighLighterAttributes.Create(SYNS_AttrTriangle);
  fTriangleAttri.Foreground := clBlack;
  AddAttribute(fTriangleAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterLDraw;
  fRange := rsUnknown;
end;

procedure TSynLDRSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynLDRSyn.FirstChar(DatLine: PChar): Char;
var
  index: Integer;

begin
  index := 0;
  while DATLine[index] = ' ' do inc(index);
  Result := DATLine[index];
end;

procedure TSynLDRSyn.IdentProc;
begin
  if FirstChar(fLine) = '0' then
  begin
    fTokenID := tkComment;
    while (fLine[Run] <> #10) and (fLine[Run] <> #13)
          and (fLine[Run] <> #0) do inc(Run);
  end
  else
  begin
    fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
  end;
end;

procedure TSynLDRSyn.Number1Proc;
  function ArgNumber(DatLine: PChar): Byte;

  var
   index: Integer;
   flag: Boolean;

  begin
    index := 0;
    Result := 0;
    flag := false;
    while index <= Run do 
    begin
      if DatLine[index] = ' ' then 
      begin
        inc(index);
        flag := false;
      end
      else
      begin
        if flag = false then inc(Result);
        flag := true;
        inc(index)
      end;
    end;
  end;

begin
  case ArgNumber(fLine) of
  
    1: begin
         case fLine[Run] of
           '0': fTokenID := tkComment;
           '1': fTokenID := tkIdentifier;
           '2': fTokenID := tkLine;
           '3': fTokenID := tkTriangle;
           '4': fTokenID := tkQuad;
           '5': fTokenID := tkOpLine;
         end;
       end; 
    2: if FirstChar(fLine) <> '0' then fTokenID := tkColor 
         else fTokenID := tkComment; 
    3..5: if FirstChar(fLine) <> '0' then fTokenID := tkFirstTri
            else fTokenID := tkComment; 
    6..8: if FirstChar(fLine) <> '0' then fTokenID := tkSecondTri
            else fTokenID := tkComment; 
    9..11: if FirstChar(fLine) <> '0' then fTokenID := tkThirdTri
             else fTokenID := tkComment; 
    12..14: if FirstChar(fLine) <> '0' then fTokenID := tkFourthTri
             else fTokenID := tkComment; 
    else
      fTokenID := tkIdentifier;
  end;
  while FLine[Run] in ['0'..'9', '.'] do inc(Run);
end;

procedure TSynLDRSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynLDRSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynLDRSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
  else
    Result := nil;
  end;
end;

function TSynLDRSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynLDRSyn.GetKeyWords: string;
begin
  Result := 
    'Author';
end;

function TSynLDRSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynLDRSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLDRSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkColor: Result := fColorAttri;
    tkComment: Result := fCommentAttri;
    tkFirstTri: Result := fFirstTriAttri;
    tkFourthTri: Result := fFourthTriAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkLine: Result := fLineAttri;
    tkOpLine: Result := fOpLineAttri;
    tkQuad: Result := fQuadAttri;
    tkSecondTri: Result := fSecondTriAttri;
    tkThirdTri: Result := fThirdTriAttri;
    tkTriangle: Result := fTriangleAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynLDRSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynLDRSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynLDRSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z', '0'..'9'];
end;

function TSynLDRSyn.GetSampleSource: string;
begin
  Result := #13#10 +
            'Sample source for: '#13#10 +
            'Ldraw Parser/Highlighter'#13#10 +
            '0 Comment'#13#10 +
            '1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat'#13#10 +
            '2 16 0 0 0 1 1 1'#13#10 +
            '3 16 0 0 0 1 1 1 2 2 2'#13#10 +
            '4 16 0 0 0 1 1 1 2 2 2 3 3 3'#13#10 +
            '5 16 0 0 0 1 1 1 2 2 2 3 3 3';
end;

function TSynLDRSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterLDraw;
end;

class function TSynLDRSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLDraw;
end;

procedure TSynLDRSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynLDRSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynLDRSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynLDRSyn);
{$ENDIF}
end.
