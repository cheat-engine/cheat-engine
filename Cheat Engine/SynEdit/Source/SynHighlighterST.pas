{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterST.pas, released 2002-07.
ST stands for Structured Text, and it is part of IEC1131 standard for
programming PLCs.
Author of this file is Ruggero Bandera.
Portions created by Ruggero Bandera are Copyright (C) 2002 Ruggero Bandera.
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

$Id: SynHighlighterST.pas,v 1.10 2005/01/28 16:53:25 maelh Exp $ by Ruggero Bandera

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERST}
unit SynHighlighterST;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsUnKnown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynSTSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..207] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func158: TtkTokenKind;
    function Func165: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func171: TtkTokenKind;
    function Func182: TtkTokenKind;
    function Func185: TtkTokenKind;
    function Func193: TtkTokenKind;
    function Func206: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    procedure SetRange(Value: Pointer); override;
    property IdentChars;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
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
    Case I of
      'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
    else mHashTable[Char(I)] := 0;
    end;
  end;
end;

procedure TSynSTSyn.InitIdent;
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
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[152] := Func152;
  fIdentFuncTable[155] := Func155;
  fIdentFuncTable[158] := Func158;
  fIdentFuncTable[165] := Func165;
  fIdentFuncTable[168] := Func168;
  fIdentFuncTable[171] := Func171;
  fIdentFuncTable[182] := Func182;
  fIdentFuncTable[185] := Func185;
  fIdentFuncTable[193] := Func193;
  fIdentFuncTable[206] := Func206;
end;

function TSynSTSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9','a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynSTSyn.KeyComp(const aKey: string): Boolean;
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
end; { KeyComp }

function TSynSTSyn.Func15: TtkTokenKind;
begin
  if KeyComp('IF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func19: TtkTokenKind;
begin
  if KeyComp('DO') then Result := tkKey else
    if KeyComp('AND') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func21: TtkTokenKind;
begin
  if KeyComp('OF') then Result := tkKey else
      if KeyComp('AT') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func27: TtkTokenKind;
begin
  if KeyComp('BY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func28: TtkTokenKind;
begin
      if KeyComp('CASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func29: TtkTokenKind;
begin
  if KeyComp('ON') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func32: TtkTokenKind;
begin
  if KeyComp('LABEL') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynSTSyn.Func33: TtkTokenKind;
begin
  if KeyComp('OR') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynSTSyn.Func35: TtkTokenKind;
begin
  if KeyComp('TO') then Result := tkKey else
      Result := tkIdentifier;
end;

function TSynSTSyn.Func36: TtkTokenKind;
begin
  if KeyComp('REAL') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func39: TtkTokenKind;
begin
  if KeyComp('FOR') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynSTSyn.Func40: TtkTokenKind;
begin
  if KeyComp('ANY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ELSE') then Result := tkKey else
    if KeyComp('VAR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func43: TtkTokenKind;
begin
  if KeyComp('INT') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynSTSyn.Func44: TtkTokenKind;
begin
  if KeyComp('BOOL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func47: TtkTokenKind;
begin
  if KeyComp('THEN') then Result := tkKey else
      if KeyComp('DINT') then Result := tkKey else
          if KeyComp('TIME') then Result := tkKey else
              Result := tkIdentifier;
end;

function TSynSTSyn.Func49: TtkTokenKind;
begin
  if KeyComp('NOT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func51: TtkTokenKind;
begin
  if KeyComp('ELSIF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func52: TtkTokenKind;
begin
  if KeyComp('BYTE') then Result := tkKey else
    if KeyComp('FROM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func56: TtkTokenKind;
begin
  if KeyComp('INDEX') then
  begin
    if fRange = rsProperty then Result := tkKey else Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynSTSyn.Func57: TtkTokenKind;
begin
  if KeyComp('GOTO') then Result := tkKey else
    if KeyComp('WHILE') then Result := tkKey else
      if KeyComp('XOR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func58: TtkTokenKind;
begin
  if KeyComp('EXIT') then Result := tkKey else
      Result := tkIdentifier;
end;


function TSynSTSyn.Func60: TtkTokenKind;
begin
  if KeyComp('STEP') then Result := tkKey else
      if KeyComp('WORD') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func62: TtkTokenKind;
begin
  if KeyComp('ACTION') then Result := tkKey else
    if KeyComp('SINT') then Result := tkKey else
        Result := tkIdentifier;
end;

function TSynSTSyn.Func63: TtkTokenKind;
begin
      if KeyComp('ARRAY') then Result := tkKey else
            Result := tkIdentifier;
end;

function TSynSTSyn.Func64: TtkTokenKind;
begin
  if KeyComp('DWORD') then Result := tkKey else
    if KeyComp('UINT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func65: TtkTokenKind;
begin
  if KeyComp('REPEAT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func66: TtkTokenKind;
begin
  if KeyComp('TYPE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func67: TtkTokenKind;
begin
  if KeyComp('RETAIN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func68: TtkTokenKind;
begin
  if KeyComp('UDINT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func69: TtkTokenKind;
begin
  if KeyComp('END_IF') then Result := tkKey else
      Result := tkIdentifier;
end;

function TSynSTSyn.Func76: TtkTokenKind;
begin
  if KeyComp('UNTIL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func79: TtkTokenKind;
begin
  if KeyComp('FINALLY') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func82: TtkTokenKind;
begin
  if KeyComp('END_CASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func83: TtkTokenKind;
begin
  if KeyComp('USINT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func87: TtkTokenKind;
begin
  if KeyComp('STRING') then Result := tkKey else
        Result := tkIdentifier;
end;

function TSynSTSyn.Func88: TtkTokenKind;
begin
  if KeyComp('PROGRAM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func93: TtkTokenKind;
begin
    if KeyComp('END_FOR') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func95: TtkTokenKind;
begin
  if KeyComp('END_VAR') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func96: TtkTokenKind;
begin
  if KeyComp('RETURN') then Result := tkKey else
        Result := tkIdentifier;
end;

function TSynSTSyn.Func99: TtkTokenKind;
begin
  if KeyComp('EXTERNAL') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func101: TtkTokenKind;
begin
  if KeyComp('STRUCT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func102: TtkTokenKind;
begin
  if KeyComp('FUNCTION') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func104: TtkTokenKind;
begin
  if KeyComp('RESOURCE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func106: TtkTokenKind;
begin
  if KeyComp('VAR_EXTERNAL') then Result := tkKey else
      if KeyComp('CONSTANT') then Result := tkKey else
          Result := tkIdentifier;
end;

function TSynSTSyn.Func111: TtkTokenKind;
begin
  if KeyComp('END_WHILE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func114: TtkTokenKind;
begin
  if KeyComp('END_STEP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func116: TtkTokenKind;
begin
  if KeyComp('END_ACTION') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func119: TtkTokenKind;
begin
  if KeyComp('ANY_NUM') then Result := tkKey else
      if KeyComp('END_REPEAT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func120: TtkTokenKind;
begin
  if KeyComp('END_TYPE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func121: TtkTokenKind;
begin
  if KeyComp('VAR_GLOBAL') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func139: TtkTokenKind;
begin
  if KeyComp('TRANSITION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func152: TtkTokenKind;
begin
  if KeyComp('CONFIGURATION') then Result := tkKey else
      if KeyComp('VAR_INPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func155: TtkTokenKind;
begin
  if KeyComp('END_STRUCT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func158: TtkTokenKind;
begin
  if KeyComp('END_RESOURCE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func165: TtkTokenKind;
begin
  if KeyComp('INITIAL_STEP') then Result := tkKey else
        Result := tkIdentifier;
end;


function TSynSTSyn.Func168: TtkTokenKind;
begin
  if KeyComp('INITIALIZATION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func171: TtkTokenKind;
begin
  if KeyComp('VAR_EXTERNAL') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynSTSyn.Func182: TtkTokenKind;
begin
   if KeyComp('VAR_IN_OUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func185: TtkTokenKind;
begin
   if KeyComp('VAR_OUTPUT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.Func193: TtkTokenKind;
begin
  if KeyComp('END_TRANSITION') then Result := tkKey else Result := tkIdentifier;
end;


function TSynSTSyn.Func206: TtkTokenKind;
begin
  if KeyComp('END_CONFIGURATION') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSTSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynSTSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 207 then Result := fIdentFuncTable[HashKey] else
    Result := tkIdentifier;
end;

procedure TSynSTSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '#': fProcTable[I] := AsciiCharProc;
      '$': fProcTable[I] := IntegerProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := BraceOpenProc;
      '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := RoundOpenProc;
            '.': fProcTable[I] := PointProc;
            ';': fProcTable[I] := SemicolonProc;
            '/': fProcTable[I] := SlashProc;
            ':', '>': fProcTable[I] := ColonOrGreaterProc;
            '<': fProcTable[I] := LowerProc;
            '@': fProcTable[I] := AddressOpProc;
          else
            fProcTable[I] := SymbolProc;
          end;
        end;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynSTSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler);
  AddAttribute(fAsmAttri);
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
  fDefaultFilter := SYNS_FilterST;
end; { Create }

procedure TSynSTSyn.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynSTSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynSTSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  while FLine[Run] in ['0'..'9'] do inc(Run);
end;

procedure TSynSTSyn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then begin
          Inc(Run);
          if fRange = rsBorAsm then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynSTSyn.BraceOpenProc;
begin
  if fRange = rsAsm then
    fRange := rsBorAsm
  else
    fRange := rsBor;
  BorProc;
end;

procedure TSynSTSyn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynSTSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynSTSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynSTSyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
end;

procedure TSynSTSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSTSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '>'] then inc(Run);
end;

procedure TSynSTSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSTSyn.NumberProc;
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

procedure TSynSTSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['.', ')'] then inc(Run);
end;

procedure TSynSTSyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then
          fRange := rsAsm
        else
          fRange := rsUnKnown;
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynSTSyn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not (fLine[Run] in [#0, #10, #13]) then
          AnsiProc;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSTSyn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange = rsProperty then
    fRange := rsUnknown;
end;

procedure TSynSTSyn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else
    fTokenID := tkSymbol;
end;

procedure TSynSTSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynSTSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do begin
    if fLine[Run] = #39 then begin
      Inc(Run);
      if fLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSTSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSTSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSTSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm:
      BorProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynSTSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
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

function TSynSTSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSTSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynSTSyn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynSTSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSTSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynSTSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynSTSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynSTSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynSTSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

function TSynSTSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynSTSyn.GetLanguageName: string;
begin
  Result := SYNS_LangST;
end;

function TSynSTSyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterST;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSTSyn);
{$ENDIF}
end.
