{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHaskell.pas, released 2001-10-28
The Original Code is based on the SynHighlighterCpp.pas, released 2000-04-10
which in turn was based on the dcjCppSyn.pas file from the mwEdit component
suite by Martin Waldenburg and other developers, the Initial Author of this file
is Michael Trier. All Rights Reserved.

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

-------------------------------------------------------------------------------}
{
@abstract(Provides a Haskell syntax highlighter for SynEdit)
@author(Ashley Brown)
@created(2001)
@lastmod(2000-10-26)
The SynHighlighterHaskell unit provides SynEdit with a Haskell syntax highlighter.
Based on SynHighlighterCpp.

http://haskell.org/
http://www.ashleybrown.co.uk/
ashley@ashleybrown.co.uk
}

{$IFNDEF QSYNHIGHLIGHTERHASKELL}
unit SynHighlighterHaskell;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
    rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynHaskellSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fLineNumber: Integer;
    fIdentFuncTable: array[0..206] of TIdentFuncTableFunc;

    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;

    function Func17: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind; 
    function Func64: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind; 
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func131: TtkTokenKind;

    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetSampleSource: string; override;
    function GetIdentChars: TSynIdentChars; override;
    function GetExtTokenID: TxtkTokenKind;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
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
  Windows,
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z', #39:
        Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;
    Case I in['_', 'a'..'z', 'A'..'Z'] of
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

procedure TSynHaskellSyn.InitIdent;
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
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[24] := Func24;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;  
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[131] := Func131;
end;

function TSynHaskellSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynHaskellSyn.KeyComp(const aKey: String): Boolean;
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
  end
  else
    Result := False;
end; { KeyComp }

function TSynHaskellSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func24: TtkTokenKind;
begin
  if KeyComp('IO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func25: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func30: TtkTokenKind;
begin
  if KeyComp('data') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func33: TtkTokenKind;
begin
  if KeyComp('Char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func40: TtkTokenKind;
begin
  if KeyComp('let') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func45: TtkTokenKind;
begin
  if KeyComp('Int') then Result := tkKey else
    if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func47: TtkTokenKind;
begin
  if KeyComp('False') then Result := tkKey else
    if KeyComp('Bool') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func51: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func58: TtkTokenKind;
begin
  if KeyComp('Float') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func59: TtkTokenKind;
begin
  if KeyComp('class') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func64: TtkTokenKind;
begin
  if KeyComp('where') then Result := tkKey else
    if KeyComp('Double') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func67: TtkTokenKind;
begin
  if KeyComp('True') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func70: TtkTokenKind;
begin
  if KeyComp('type') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func76: TtkTokenKind;
begin
  if KeyComp('module') then Result := tkKey else Result := tkIdentifier;
end;


function TSynHaskellSyn.Func84: TtkTokenKind;
begin
  if KeyComp('Integer') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func92: TtkTokenKind;
begin
  if KeyComp('String') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func93: TtkTokenKind;
begin
  if KeyComp('instance') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func96: TtkTokenKind;
begin
  if KeyComp('deriving') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func97: TtkTokenKind;
begin
  if KeyComp('import') then Result := tkKey else Result := tkIdentifier;
end;

function TSynHaskellSyn.Func131: TtkTokenKind;
begin
  if KeyComp('otherwise') then Result := tkKey else Result := tkIdentifier;
end;


function TSynHaskellSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynHaskellSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 207 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynHaskellSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := AsciiCharProc;
      '@': fProcTable[I] := AtSymbolProc;
      '}': fProcTable[I] := BraceCloseProc;
      '{': fProcTable[I] := BraceOpenProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := ColonProc;
      ',': fProcTable[I] := CommaProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
      '?': fProcTable[I] := QuestionProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '%': fProcTable[I] := ModSymbolProc;
      '!': fProcTable[I] := NotSymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '.': fProcTable[I] := PointProc;
      ')': fProcTable[I] := RoundCloseProc;
      '(': fProcTable[I] := RoundOpenProc;
      ';': fProcTable[I] := SemiColonProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      ']': fProcTable[I] := SquareCloseProc;
      '[': fProcTable[I] := SquareOpenProc;
      '*': fProcTable[I] := StarProc;
      #34: fProcTable[I] := StringProc;
      '~': fProcTable[I] := TildeProc;
      '^': fProcTable[I] := XOrSymbolProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynHaskellSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  fSpaceAttri.Foreground := clWindow;
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterHaskell;
end; { Create }

procedure TSynHaskellSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynHaskellSyn.AnsiCProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          inc(Run, 2);
          if fRange = rsAnsiCAsm then
            fRange := rsAsm
          else if fRange = rsAnsiCAsmBlock then
            fRange := rsAsmBlock
          else if fRange = rsDirectiveComment then
            fRange := rsDirective
          else
            fRange := rsUnKnown;
          break;
        end else
          inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynHaskellSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      inc(Run);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynHaskellSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      if fLine[Run + 1] in [#39, '\'] then
        inc(Run);
    end;
    inc(Run);
  until fLine[Run] in [#0, #10, #13, #39];
  if fLine[Run] = #39 then
    inc(Run);
end;

procedure TSynHaskellSyn.AtSymbolProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

procedure TSynHaskellSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if fRange = rsAsmBlock then fRange := rsUnknown;
end;

procedure TSynHaskellSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if fRange = rsAsm then
  begin
    fRange := rsAsmBlock;
    fAsmStart := True;
  end;
end;

procedure TSynHaskellSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynHaskellSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        inc(Run, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      inc(Run);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TSynHaskellSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynHaskellSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      inc(Run);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynHaskellSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynHaskellSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TSynHaskellSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynHaskellSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynHaskellSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
      end;
  else                                 {less than}
    begin
      inc(Run);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynHaskellSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        fTokenID := tkComment;
        inc(Run, 2);
        while not (fLine[Run] in [#0, #10, #13]) do Inc(Run);
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynHaskellSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      inc(Run);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TSynHaskellSyn.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      inc(Run);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynHaskellSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynHaskellSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
    ['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynHaskellSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {or assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      inc(Run);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynHaskellSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      inc(Run);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynHaskellSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynHaskellSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynHaskellSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynHaskellSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if fRange = rsAsm then fRange := rsUnknown;
end;

procedure TSynHaskellSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '=':                               {divide assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynHaskellSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynHaskellSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynHaskellSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynHaskellSyn.StarProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      inc(Run);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TSynHaskellSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      if fLine[Run + 1] in [#34, '\'] then
        Inc(Run);
    end;
    inc(Run);
  until fLine[Run] in [#0, #10, #13, #34];
  if FLine[Run] = #34 then
    inc(Run);
end;

procedure TSynHaskellSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynHaskellSyn.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      inc(Run);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynHaskellSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynHaskellSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock: AnsiCProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynHaskellSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynHaskellSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynHaskellSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynHaskellSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynHaskellSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHaskellSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynHaskellSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    else Result := nil;
  end;
end;

function TSynHaskellSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynHaskellSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynHaskellSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynHaskellSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynHaskellSyn.EnumUserSettings(settings: TStrings);
begin
  {$IFNDEF SYN_CLX}
  { returns the user settings that exist in the registry }
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\C++Builder') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
  {$ENDIF}
end;

function TSynHaskellSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

function TSynHaskellSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterHaskell;
end;

class function TSynHaskellSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHaskell;
end;

class function TSynHaskellSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynHaskellSyn.GetSampleSource: string;
begin
  Result := '-- Haskell Sample Source'#13#10 +
            'tail :: [a] -> [a]'#13#10 +
            'tail (x:xs) = xs'#13#10 +
            '';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHaskellSyn);
{$ENDIF}
end.
