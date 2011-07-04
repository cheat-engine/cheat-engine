{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterBat.pas, released 2000-04-18.
The Original Code is based on the dmBatSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
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

$Id: SynHighlighterBat.pas,v 1.15 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a MS-DOS Batch file highlighter for SynEdit)
@author(David Muir <dhm@dmsoftware.co.uk>)
@created(Late 1999)
@lastmod(May 19, 2000)
The SynHighlighterBat unit provides SynEdit with a MS-DOS Batch file (.bat) highlighter.
The highlighter supports the formatting of keywords and parameters (batch file arguments).
}

{$IFNDEF QSYNHIGHLIGHTERBAT}
unit SynHighlighterBat;
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
    tkUnknown, tkVariable);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynBatSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..130] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func7: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func130: TtkTokenKind;
    procedure VariableProc;
    procedure CRProc;
    procedure CommentProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure REMCommentProc;
    procedure SpaceProc;
    procedure UnknownProc;
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
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
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
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynBatSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[7] := Func7;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[130] := Func130;
end;

function TSynBatSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynBatSyn.KeyComp(const aKey: String): Boolean;
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

function TSynBatSyn.Func7: TtkTokenKind;
begin
  if KeyComp('cd') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func19: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func21: TtkTokenKind;
begin
  if KeyComp('del') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func23: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func27: TtkTokenKind;
begin
  if KeyComp('off') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func28: TtkTokenKind;
begin
  if KeyComp('call') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func29: TtkTokenKind;
begin
  if KeyComp('on') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func31: TtkTokenKind;
begin
  if KeyComp('echo') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func34: TtkTokenKind;
begin
  if KeyComp('cls') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func39: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func44: TtkTokenKind;
begin
  if KeyComp('set') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func49: TtkTokenKind;
begin
  if KeyComp('not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func57: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func59: TtkTokenKind;
begin
  if KeyComp('copy') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func62: TtkTokenKind;
begin
  if KeyComp('shift') then Result := tkKey else
    if KeyComp('pause') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func66: TtkTokenKind;
begin
  if KeyComp('title') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func77: TtkTokenKind;
begin
  if KeyComp('exist') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func78: TtkTokenKind;
begin
  if KeyComp('start') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.Func130: TtkTokenKind;
begin
  if KeyComp('errorlevel') then Result := tkKey else Result := tkIdentifier;
end;

function TSynBatSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynBatSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 131 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynBatSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '%': fProcTable[I] := VariableProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := CommentProc;
      'A'..'Q', 'S'..'Z', 'a'..'q', 's'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'R', 'r': fProcTable[I] := REMCommentProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynBatSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  fVariableAttri.Foreground := clGreen;
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterBatch;
end;

procedure TSynBatSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynBatSyn.VariableProc;
begin
  fTokenID := tkVariable;
  repeat
    Inc(Run);
  until not (fLine[Run] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  if fLine[Run] = '%' then
    Inc(Run);
end;

procedure TSynBatSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if (fLine[Run] = #10) then Inc(Run);
end;

procedure TSynBatSyn.CommentProc;
begin
  fTokenID := tkIdentifier;
  Inc(Run);
  if fLine[Run] = ':' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until (fLine[Run] in [#0, #10, #13]);
  end;
end;

procedure TSynBatSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynBatSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynBatSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynBatSyn.NumberProc;
begin
  fTokenID := tkNumber;
  repeat
    Inc(Run);
  until not (fLine[Run] in ['0'..'9', '.']);
end;

procedure TSynBatSyn.REMCommentProc;
begin
  if (FLine[Run+1] in ['E','e']) and (FLine[Run+2] in ['M','m'])
    and (FLine[Run+3] < #33) then
  begin
    fTokenID := tkComment;
    Inc(Run, 3);
    while (FLine[Run] <> #0) do begin
      case FLine[Run] of
        #10, #13: break;
      end; { case }
      Inc(Run);
    end; { while }
  end
  else
  begin
    fTokenID := tkIdentifier;
    IdentProc;
  end;
end;

procedure TSynBatSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynBatSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynBatSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynBatSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynBatSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynBatSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynBatSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkVariable: Result := fVariableAttri;
    else Result := nil;
  end;
end;

function TSynBatSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynBatSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynBatSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynBatSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynBatSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterBatch;
end;

class function TSynBatSyn.GetLanguageName: string;
begin
  Result := SYNS_LangBatch;
end;

function TSynBatSyn.GetSampleSource: string;
begin
  Result := 'rem MS-DOS batch file'#13#10 +
            'rem'#13#10 +
            '@echo off'#13#10 +
            'cls'#13#10 +
            'echo The command line is: %1 %2 %3 %4 %5'#13#10 +
            'rem'#13#10 +
            'rem now wait for the user ...'#13#10 +
            'pause'#13#10 +
            'copy c:\*.pas d:\'#13#10 +
            'if errorlevel 1 echo Error in copy action!';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynBatSyn);
{$ENDIF}
end.
