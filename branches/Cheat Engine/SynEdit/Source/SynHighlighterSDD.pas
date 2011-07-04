{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSDD.pas, released 2001-08-20.
The Initial Author of this file is Pieter Polak.
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

$Id: SynHighlighterSDD.pas,v 1.14 2005/01/28 16:53:25 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERSDD}
unit SynHighlighterSDD;
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
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkDatatype,
    tkNumber,
    tkNull,
    tkSpace,
    tkSymbol,
    tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TRangeState = (rsComment, rsUnKnown);

type
  TSynSDDSyn = class(TSynCustomHighlighter)
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
    fIdentFuncTable: array[0..141] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fDatatypeAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func141: TtkTokenKind;
    procedure BraceOpenProc;
    procedure BraceCommentProc;
    procedure NumberProc;                                                    
    procedure CRProc;
    procedure LFProc;
    procedure IdentProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure UnknownProc;
    procedure SymbolProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
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
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property DatatypeAttri: TSynHighlighterAttributes read fDatatypeAttri write fDatatypeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
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
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynSDDSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[141] := Func141;
end;

function TSynSDDSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynSDDSyn.KeyComp(const aKey: String): Boolean;
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

function TSynSDDSyn.Func21: TtkTokenKind;
begin
  if KeyComp('of') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func23: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func30: TtkTokenKind;
begin
  if KeyComp('date') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func36: TtkTokenKind;
begin
  if KeyComp('real') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func41: TtkTokenKind;
begin
  if KeyComp('var') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func43: TtkTokenKind;
begin
  if KeyComp('spec') then Result := tkKey else
    if KeyComp('block') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func47: TtkTokenKind;
begin
  if KeyComp('time') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func52: TtkTokenKind;
begin
  if KeyComp('byte') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func53: TtkTokenKind;
begin
  if KeyComp('database') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func55: TtkTokenKind;
begin
  if KeyComp('object') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func60: TtkTokenKind;
begin
  if KeyComp('keys') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func63: TtkTokenKind;
begin
  if KeyComp('array') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func66: TtkTokenKind;
begin
  if KeyComp('endblock') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func74: TtkTokenKind;
begin
  if KeyComp('objects') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func75: TtkTokenKind;
begin
  if KeyComp('owner') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func78: TtkTokenKind;
begin
  if KeyComp('integer') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func87: TtkTokenKind;
begin
  if KeyComp('string') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func91: TtkTokenKind;
begin
  if KeyComp('longint') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func95: TtkTokenKind;
begin
  if KeyComp('binarydata') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func100: TtkTokenKind;
begin
  if KeyComp('primary') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func104: TtkTokenKind;
begin
  if KeyComp('secondary') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func115: TtkTokenKind;
begin
  if KeyComp('memotext') then Result := tkDatatype else Result := tkIdentifier;
end;

function TSynSDDSyn.Func122: TtkTokenKind;
begin
  if KeyComp('partition') then Result := tkKey else
    if KeyComp('superspec') then Result := tkKey else
      if KeyComp('superblock') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.Func141: TtkTokenKind;
begin
  if KeyComp('partitions') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSDDSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSDDSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 142 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynSDDSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '{'       : fProcTable[I] := BraceOpenProc;
      '}',
      '!',
      '%',
      '&',
      '('..'/',
      ':'..'@',
      '['..'^',
      '`', '~'  : fProcTable[I] := SymbolProc;
      'A'..'Z',
      'a'..'z',
      '_'       : fProcTable[I] := IdentProc;
      '0'..'9'  : fProcTable[I] := NumberProc;
      #0        : fProcTable[I] := NullProc;
      #1..#32   : fProcTable[I] := SpaceProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end; { MakeMethodTables }


constructor TSynSDDSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clNavy;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);

  fDatatypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
  fDatatypeAttri.Style := [fsBold];
  fDatatypeAttri.Foreground := clTeal;
  AddAttribute(fDatatypeAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterSDD;
  fRange := rsUnknown;
end; { Create }


procedure TSynSDDSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }


procedure TSynSDDSyn.BraceOpenProc;
begin
  fRange := rsComment;
  BraceCommentProc;
  fTokenID := tkComment;
end; { BraceOpenProc }


procedure TSynSDDSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end; { IdentProc }


procedure TSynSDDSyn.NullProc;
begin
  fTokenID := tkNull;
end; { NullProc }


procedure TSynSDDSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end; { SpaceProc }


procedure TSynSDDSyn.BraceCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then
        begin
          Inc(Run);
          fRange := rsUnKnown;
          Break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end; { BraceCommentProc }


procedure TSynSDDSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end; { UnknownProc }


procedure TSynSDDSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: BraceCommentProc;
  else
    fProcTable[fLine[Run]];
  end;
end; { Next }


procedure TSynSDDSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end; { CRProc }


procedure TSynSDDSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end; { LFProc }


function TSynSDDSyn.GetSampleSource: string;
begin
  Result := '{ Semanta data dictionary }'#13#10 +
            'database Sample.001;'#13#10 +
            'owner = COAS;'#13#10 +
            #13#10 +
            'objects'#13#10 +
            '  Test = object'#13#10 +
            '    Code : string[4];'#13#10 +
            '    Name : string[80];'#13#10 +
            '  end;'#13#10 +
            'keys'#13#10 +
            '  primary Test.Index = [Code];'#13#10 +
            'end.';
end; { GetSampleSource }


function TSynSDDSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD   : Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL    : Result := fSymbolAttri;
  else
    Result := nil;
  end;
end; { GetDefaultAttribute }


function TSynSDDSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end; { GetEol }


function TSynSDDSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end; { GetToken }


function TSynSDDSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end; { GetTokenId }


function TSynSDDSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkDatatype: Result := fDatatypeAttri;
    tkSpace: Result := fSpaceAttri;
    tkNumber: Result := fNumberAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSymbol: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }


function TSynSDDSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end; { GetTokenKind }


function TSynSDDSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end; { GetTokenPos }


procedure TSynSDDSyn.ResetRange;
begin
  inherited;
  fRange := rsUnknown;
end; { ResetRange }


procedure TSynSDDSyn.SetRange(Value: Pointer);
begin
  inherited;
  fRange := TRangeState(Value);
end; { SetRange }


function TSynSDDSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end; { GetRange }


function TSynSDDSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end; { GetIdentChars }

class function TSynSDDSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSDD;
end; { GetLanguageName }


procedure TSynSDDSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.': if FLine[Run + 1] = '.' then
             Break;
    end;
    inc(Run);
  end;
end; { NumberProc }


function TSynSDDSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterSDD;
end; { IsFilterStored }


procedure TSynSDDSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            
  RegisterPlaceableHighlighter(TSynSDDSyn);
{$ENDIF}
end.
