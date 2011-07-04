{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHC11.pas, released 2000-04-21.
The Original Code is based on the CIHC11Syn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Nils Springob.
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

$Id: SynHighlighterHC11.pas,v 1.14 2005/01/28 16:53:22 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a 68HC11 Assembler Language syntax highlighter for SynEdit)
@author(Nils Springob <delphi.nils@crazy-idea.de>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(January 2000, converted to SynEdit April 21, 2000)
@lastmod(2000-06-23)
The SynHighlighterHC11 unit provides SynEdit with a 68HC11 Assembler (.asm) highlighter.
The highlighter supports all 68HC11 op codes.
Thanks to Martin Waldenburg, David Muir, Hideo Koiso and Nick Hoddinott.
}

{$IFNDEF QSYNHIGHLIGHTERHC11}
unit SynHighlighterHC11;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
  QSynEditTypes,
{$ELSE}
  Graphics,
  SynEditHighlighter,
  SynEditTypes,
{$ENDIF}
  SysUtils,
  Classes;

const
  KeyWordCount = 149;
  DirectiveCount = 6;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TkwKeyWordType = (kwNone, kwOperand, kwOperandOver, kwNoOperand);

  PHashListEntry = ^THashListEntry;
  THashListEntry = record
    Next: PHashListEntry;
    Token: String;
    Kind: TtkTokenKind;
    Op: Boolean;
  end;

  TProcTableProc = procedure of Object;

  TSynHC11Syn = class(TSynCustomHighLighter)
  private
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FKeyWordType: TkwKeyWordType;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;

    fHashArray: array[0..DirectiveCount+KeyWordCount] of THashListEntry;
    fHashList: array[#0..#255] of PHashListEntry;
    fHashArrayIndex: integer;

    function KeyHash(ToHash: PChar): char;
    function KeyComp(const aKey: string): Boolean;

    procedure SymAsciiCharProc;
    procedure SymbolProc;
    procedure SymDollarProc;
    procedure SymCRProc;
    procedure SymIdentProc;
    procedure SymLFProc;
    procedure SymPercentProc;
    procedure SymNullProc;
    procedure SymNumberProc;
    procedure SymSpaceProc;
    procedure SymStarProc;
    procedure SymStringProc;
    procedure SymUnknownProc;

    procedure InitIdent;
    procedure MakeMethodTables;
    procedure AddHashEntry(NewToken: String; NewKind: TtkTokenKind);
    function IdentKind(MayBe: PChar): TtkTokenKind;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
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
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
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

const
  KeyWords: array[1..KeyWordCount] of string = (
    'ABA', 'ABX', 'ABY', 'ADCA_', 'ADCB_', 'ADDA_', 'ADDB_', 'ADDD_', 'ANDA_',
    'ANDB_', 'ASLA', 'ASLB', 'ASL_', 'ASLD', 'ASRA', 'ASRB', 'ASR_', 'BCC_',
    'BCLR_', 'BCS_', 'BEQ_', 'BGE_', 'BGT_', 'BHI_', 'BHS_', 'BITA_', 'BITB_',
    'BLE_', 'BLO_', 'BLS_', 'BLT_', 'BMI_', 'BNE_', 'BPL_', 'BRA_', 'BRCLR_',
    'BRN_', 'BRSET_', 'BSET_', 'BSR_', 'BVC_', 'BVS_', 'CBA', 'CLC', 'CLI',
    'CLRA', 'CLRB', 'CLR_', 'CLV', 'CMPA_', 'CMPB_', 'COMA', 'COMB', 'COM_',
    'CPD_', 'CPX_', 'CPY_', 'DAA', 'DECA', 'DECB', 'DEC_', 'DES', 'DEX', 'DEY',
    'EORA_', 'EORB_', 'FDIV', 'IDIV', 'INCA', 'INCB', 'INC_', 'INS', 'INX',
    'INY', 'JMP_', 'JSR_', 'LDAA_', 'LDAB_', 'LDD_', 'LDS_', 'LDX_', 'LDY_',
    'LSLA', 'LSLB', 'LSL_', 'LSLD', 'LSRA', 'LSRB', 'LSR_', 'LSRD', 'MUL',
    'NEGA', 'NEGB', 'NEG_', 'NOP', 'ORAA_', 'ORAB_', 'PSHA', 'PSHB', 'PSHX',
    'PSHY', 'PULA', 'PULB', 'PULX', 'PULY', 'ROLA', 'ROLB', 'ROL_', 'RORA',
    'RORB', 'ROR_', 'RTI', 'RTS', 'SBA', 'SBCA_', 'SBCB_', 'SEC', 'SEI', 'SEV',
    'STAA_', 'STAB_', 'STD_', 'STOP', 'STS_', 'STX_', 'STY_', 'SUBA_', 'SUBB_',
    'SUBD_', 'SWI', 'TAB', 'TAP', 'TBA', 'TEST', 'TPA', 'TSTA', 'TSTB', 'TST_',
    'TSX', 'TSY', 'TXS', 'TYS', 'WAI', 'XGDX', 'XGDY', // end commands
    'FCC_','FCB_','BSZ_','FDB_' // codegenerating directives
    );

  Directives: array[1..DirectiveCount] of string = (
    'EQU_', 'OPT_', 'PAGE', 'ORG_', 'RMB_', 'END'  // directives
    );
procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
  end;
end;


procedure TSynHC11Syn.AddHashEntry(NewToken: String; NewKind: TtkTokenKind);
var
  Hash: char;
  actEntryPtr: ^PHashListEntry;
  actEntry: PHashListEntry;
begin
  with fHashArray[fHashArrayIndex] do
  begin
    Op := false;
    if Pos('_', NewToken) <> 0 then begin
      Delete(NewToken, Length(NewToken), 1);
      Op := true;
    end;
    Token := NewToken;
    Kind := NewKind;
    Hash := KeyHash(PChar(NewToken));
    actEntryPtr := @fHashList[Hash];
    actEntry := fHashList[Hash];
    while (actEntry <> nil) do
    begin
      actEntryPtr := @actEntry.Next;
      actEntry := actEntry.Next;
    end;
    Next := nil;
  end;
  actEntryPtr^ := @fHashArray[fHashArrayIndex];
  inc(fHashArrayIndex);
end;

procedure TSynHC11Syn.InitIdent;
var
  i: integer;
  c: char;
begin
  // initialize list
  fHashArrayIndex := 0;
  for c := #0 to #255 do
    fHashList[c] := nil;
  // put all keywords into the list
  for i := 1 to KeyWordCount do
    AddHashEntry(KeyWords[i], tkKey);
  // put all directives into the list
  for i := 1 to DirectiveCount do
    AddHashEntry(Directives[i], tkDirective);
end;

function TSynHC11Syn.KeyHash(ToHash: PChar): char;
begin
  Result := #0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    Result := char(byte(Result) + byte(ToHash^));
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynHC11Syn.KeyComp(const aKey: String): Boolean;
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
  end else Result := False;
end; { KeyComp }

function TSynHC11Syn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Hash: char;
  actEntry: PHashListEntry;
begin
  fToIdent := MayBe;
  Hash := KeyHash(MayBe);

  Result := tkIdentifier;

  actEntry := fHashList[Hash];
  while (actEntry <> nil) do
  begin
    if KeyComp(actEntry.Token) then
    begin
      Result := actEntry.Kind;
      if actEntry.Op then
        FKeyWordType := kwOperand
      else
        FKeyWordType := kwNoOperand;
    end;
    actEntry := actEntry.Next;
  end;
end;

procedure TSynHC11Syn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #39: fProcTable[I] := SymAsciiCharProc;
      '$': fProcTable[I] := SymDollarProc;
      #13: fProcTable[I] := SymCRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := SymIdentProc;
      #10: fProcTable[I] := SymLFProc;
      '%': fProcTable[I] := SymPercentProc;
      #0: fProcTable[I] := SymNullProc;
      '0'..'9': fProcTable[I] := SymNumberProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SymSpaceProc;
      '*': fProcTable[I] := SymStarProc;
      #34: fProcTable[I] := SymStringProc;
      '#', ':', ',', ';', '(', ')': fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := SymUnknownProc;
    end;
end;

constructor TSynHC11Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);

  MakeMethodTables;
  InitIdent;
  fDefaultFilter := SYNS_FilterAsm68HC11;
end; { Create }

procedure TSynHC11Syn.SetLine(NewValue: string; LineNumber: Integer);
begin
  FKeyWordType := kwNone;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynHC11Syn.SymAsciiCharProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
      begin
        FKeyWordType:=kwNone;
        break;
      end;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynHC11Syn.SymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynHC11Syn.SymDollarProc;
begin
  fTokenID := tkNumber;
  inc(Run);
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    inc(Run);
end;

procedure TSynHC11Syn.SymCRProc;
begin
  fTokenID := tkSpace;
  FKeyWordType := kwNone;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynHC11Syn.SymIdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynHC11Syn.SymLFProc;
begin
  FKeyWordType := kwNone;
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynHC11Syn.SymPercentProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'1'] do
    inc(Run);
end;

procedure TSynHC11Syn.SymNullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynHC11Syn.SymNumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9'] do
    inc(Run);
end;

procedure TSynHC11Syn.SymSpaceProc;
begin
  inc(Run);
  if FKeyWordType in [kwOperandOver, kwNoOperand] then begin
    FKeyWordType := kwNone;
    fTokenID := tkComment;
    while not (fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end else begin
    if FKeyWordType = kwOperand then
      FKeyWordType := kwOperandOver;
    fTokenID := tkSpace;
    while (fLine[Run] <= #32) and not (fLine[Run] in [#0, #10, #13]) do
      inc(Run);
  end;
end;

procedure TSynHC11Syn.SymStarProc;
begin
  inc(Run);
  if FKeyWordType = kwOperandOver then
    fTokenID := tkSymbol
  else begin
    fTokenID := tkComment;
    while not (fLine[Run] in [#0, #10, #13]) do
      inc(Run);
  end;
end;

procedure TSynHC11Syn.SymStringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynHC11Syn.SymUnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynHC11Syn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynHC11Syn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynHC11Syn.GetEol: Boolean;
begin
  Result := (fTokenId = tkNull);
end;

function TSynHC11Syn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynHC11Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
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

function TSynHC11Syn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynHC11Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHC11Syn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynHC11Syn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynHC11Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterAsm68HC11;
end;

class function TSynHC11Syn.GetLanguageName: string;
begin
  Result := SYNS_Lang68HC11;
end;

function TSynHC11Syn.GetSampleSource: string;
begin
  Result :=
    '* TX.ASM'#13#10 +
    'MAINORG EQU_    $F800'#13#10 +
    '        ORG     $F800'#13#10 +
    'MAIN    EQU     *        ;Start assembling here'#13#10 +
    '        STAA    SCCR2'#13#10 +
    'loop:'#13#10 +
    '        LDAA    #$05'#13#10 +
    '	BRA	loop		;Do it again'#13#10 +
    '	ORG	$FFFE		;Reset vector interrupt setup'#13#10 +
    '	END';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHC11Syn);
{$ENDIF}
end.
