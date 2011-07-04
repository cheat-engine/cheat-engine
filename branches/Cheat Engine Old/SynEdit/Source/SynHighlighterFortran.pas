{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterFortran.pas, released 2000-04-21.
The Original Code is based on the mwFortranSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is "riceball".
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

$Id: SynHighlighterFortran.pas,v 1.16 2005/01/28 16:53:22 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Fortran syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterFortran unit provides SynEdit with a Fortran syntax highlighter.
Thanks to Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERFORTRAN}
unit SynHighlighterFortran;
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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynFortranSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..145] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func145: TtkTokenKind;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure ExclamationProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure CommentProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
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
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynFortranSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[145] := Func145;
end;

function TSynFortranSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynFortranSyn.KeyComp(const aKey: String): Boolean;
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
  end else Result := False;
end;

function TSynFortranSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func19: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func23: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func26: TtkTokenKind;
begin
  if KeyComp('data') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func28: TtkTokenKind;
begin
  if KeyComp('call') then Result := tkKey else
    if KeyComp('case') then Result := tkKey else
      if KeyComp('read') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func30: TtkTokenKind;
begin
  if KeyComp('map') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func36: TtkTokenKind;
begin
  if KeyComp('real') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func38: TtkTokenKind;
begin
  if KeyComp('endif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func41: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func42: TtkTokenKind;
begin
  if KeyComp('enddo') then Result := tkKey else
    if KeyComp('enddo') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func47: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey else
    if KeyComp('save') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func48: TtkTokenKind;
begin
  if KeyComp('cycle') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func50: TtkTokenKind;
begin
  if KeyComp('open') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func56: TtkTokenKind;
begin
  if KeyComp('elseif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func57: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func58: TtkTokenKind;
begin
  if KeyComp('exit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func59: TtkTokenKind;
begin
  if KeyComp('logical') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func61: TtkTokenKind;
begin
  if KeyComp('value') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func63: TtkTokenKind;
begin
  if KeyComp('record') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func64: TtkTokenKind;
begin
  if KeyComp('select') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func66: TtkTokenKind;
begin
  if KeyComp('type') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func68: TtkTokenKind;
begin
  if KeyComp('include') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func69: TtkTokenKind;
begin
  if KeyComp('allocate') then Result := tkKey else
    if KeyComp('default') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func70: TtkTokenKind;
begin
  if KeyComp('stop') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func73: TtkTokenKind;
begin
  if KeyComp('union') then Result := tkKey else
    if KeyComp('common') then Result := tkKey else
      if KeyComp('format') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func75: TtkTokenKind;
begin
  if KeyComp('write') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func77: TtkTokenKind;
begin
  if KeyComp('character') then Result := tkKey else
    if KeyComp('print') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func78: TtkTokenKind;
begin
  if KeyComp('integer') then Result := tkKey else
    if KeyComp('deallocate') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func81: TtkTokenKind;
begin
  if KeyComp('interface') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func82: TtkTokenKind;
begin
  if KeyComp('entry') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func84: TtkTokenKind;
begin
  if KeyComp('allocatable') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func88: TtkTokenKind;
begin
  if KeyComp('program') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func96: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func97: TtkTokenKind;
begin
  if KeyComp('parameter') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func99: TtkTokenKind;
begin
  if KeyComp('external') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func101: TtkTokenKind;
begin
  if KeyComp('continue') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func102: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey else
    if KeyComp('dimension') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func114: TtkTokenKind;
begin
  if KeyComp('equivalence') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func127: TtkTokenKind;
begin
  if KeyComp('stucture') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func144: TtkTokenKind;
begin
  if KeyComp('subroutine') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.Func145: TtkTokenKind;
begin
  if KeyComp('structure') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFortranSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynFortranSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 146 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynFortranSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #39: fProcTable[I] := AsciiCharProc;
      #13: fProcTable[I] := CRProc;
      ',': fProcTable[I] := CommaProc;
      '=': fProcTable[I] := EqualProc;
      '!': fProcTable[I] := ExclamationProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '%': fProcTable[I] := ModSymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '+': fProcTable[I] := PlusProc;
      '.': fProcTable[I] := PointProc;
      ')': fProcTable[I] := RoundCloseProc;
      '(': fProcTable[I] := RoundOpenProc;
      ';': fProcTable[I] := SemiColonProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      '*': fProcTable[I] := StarProc;
      #34: fProcTable[I] := StringProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynFortranSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterFortran;
end;

procedure TSynFortranSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynFortranSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynFortranSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynFortranSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFortranSyn.ExclamationProc;
begin
  inc(Run, 1);                        {Fortran Comments}
  fTokenID := tkComment;
  while FLine[Run] <> #0 do
  begin
    case FLine[Run] of
      #10, #13: break;
    end;
    inc(Run);
  end;
end;

procedure TSynFortranSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
          inc(Run, 3)
        else                           {shift right}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFortranSyn.IdentProc;
begin
  if (FLine[Run] in ['C', 'c']) and (Run = 0) then
  begin   //Fortran comments
    inc(Run, 1);
    CommentProc;
  end
  else begin
    fTokenID := IdentKind((fLine + Run));
    inc(Run, fStringLen);
    while Identifiers[fLine[Run]] do inc(Run);
  end;
end;

procedure TSynFortranSyn.LFProc;
begin
  inc(Run);
  fTokenID := tkSpace;
end;

procedure TSynFortranSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
          inc(Run, 3)
        else                           {shift left}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFortranSyn.MinusProc;
begin
  {subtract}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.ModSymbolProc;
begin
  {mod}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynFortranSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
      ['0'..'9', '.', 'x', 'X', 'e', 'E', 'f', 'F'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynFortranSyn.PlusProc;
begin
  {subtract}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.PointProc;
begin
  if (((UpCase(FLine[Run + 1]) = 'G') and (UpCase(FLine[Run + 2]) in ['E','T'])) {.ge. .gt.}
       or ((UpCase(FLine[Run + 1]) = 'L') and (UpCase(FLine[Run + 2]) in ['E','T'])) {.le. .lt.}
       or ((UpCase(FLine[Run + 1]) = 'N') and (UpCase(FLine[Run + 2]) = 'E')) {.ne.}
       or ((UpCase(FLine[Run + 1]) = 'E') and (UpCase(FLine[Run + 2]) = 'Q')) {.eq.}
       or ((UpCase(FLine[Run + 1]) = 'O') and (UpCase(FLine[Run + 2]) = 'R'))){.or.}
     and (FLine[Run + 3] = '.') then
    begin
      inc(Run, 4);
      fTokenID := tkSymbol;
    end
  else if (((UpCase(FLine[Run + 1]) = 'A')
              and (UpCase(FLine[Run + 2]) = 'N')
              and (UpCase(FLine[Run + 3]) = 'D'))    {.and.}
           or ((UpCase(FLine[Run + 1]) = 'N')
              and (UpCase(FLine[Run + 2]) = 'O')
              and (UpCase(FLine[Run + 3]) = 'T')))    {.not.}
          and (FLine[Run + 4] = '.') then
    begin
      inc(Run, 5);
      fTokenID := tkSymbol;
    end
  else if (UpCase(FLine[Run + 1]) = 'T')
          and (UpCase(FLine[Run + 2]) = 'R')
          and (UpCase(FLine[Run + 3]) = 'U')
          and (UpCase(FLine[Run + 4]) = 'E')
          and (FLine[Run + 5] = '.') then  {.true.}
    begin
      inc(Run, 6);
      fTokenID := tkSymbol;
    end
  else if (UpCase(FLine[Run + 1]) = 'F')
          and (UpCase(FLine[Run + 2]) = 'A')
          and (UpCase(FLine[Run + 3]) = 'L')
          and (UpCase(FLine[Run + 4]) = 'S')
          and (UpCase(FLine[Run + 5]) = 'E')
          and (FLine[Run + 6] = '.') then  {.false.}
    begin
      inc(Run, 7);
      fTokenID := tkSymbol;
    end
  else                                 {point}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
end;

procedure TSynFortranSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFortranSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.SlashProc;
begin
  {division}
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFortranSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynFortranSyn.StarProc;
begin
  if (Run = 0) then begin   //Fortran comments
    inc(Run);
    CommentProc;
  end
  else begin
    {star}
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynFortranSyn.CommentProc;
begin
  fTokenID := tkComment;
  while FLine[Run] <> #0 do
  begin
    case FLine[Run] of
      #10, #13: break;
    end; //case
    inc(Run);
  end; //while
end;

procedure TSynFortranSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynFortranSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynFortranSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynFortranSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynFortranSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynFortranSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynFortranSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynFortranSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynFortranSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynFortranSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynFortranSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynFortranSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterFortran;
end;

class function TSynFortranSyn.GetLanguageName: string;
begin
  Result := SYNS_LangFortran;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynFortranSyn);
{$ENDIF}
end.
