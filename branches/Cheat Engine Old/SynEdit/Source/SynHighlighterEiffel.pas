{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterEiffel.pas, released 2004-03-08.
Description: Eiffel Syntax Parser/Highlighter
The initial author of this file is Massimo Maria Ghisalberti (nissl).
Copyright (c) 2004, all rights reserved.

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

$Id: SynHighlighterEiffel.pas,v 1.3 2004/07/31 16:20:08 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an Eiffel highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it, nissl@linee.it - www.linee.it)
@created(03-08-2004)
@lastmod(03-08-2004)
The SynHighlighterEiffel unit provides SynEdit with an Eiffel highlighter.
}

{$IFNDEF QSYNHIGHLIGHTEREIFFEL}
unit SynHighlighterEiffel;
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
  TtkTokenKind = (
    tkBasicTypes,
    tkComment,
    tkIdentifier,
    tkKey,
    tkLace,
    tkNull,
    tkOperatorAndSymbols,
    tkPredefined,
    tkResultValue,
    tkSpace,
    tkString,
    tkUnknown);

  TRangeState = (rsUnKnown, rsEiffelComment, rsString, rsOperatorAndSymbolProc);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function :TtkTokenKind of object;

const
  MaxKey = 144;

type
  TSynEiffelSyn = class(TSynCustomHighlighter)
  private
    fLineRef :string;
    fLine :PChar;
    fLineNumber :Integer;
    fProcTable :array[#0..#255] of TProcTableProc;
    fRange :TRangeState;
    Run :LongInt;
    fStringLen :Integer;
    fToIdent :PChar;
    fTokenPos :Integer;
    fTokenID :TtkTokenKind;
    fIdentFuncTable :array[0..MaxKey] of TIdentFuncTableFunc;
    fBasicTypesAttri :TSynHighlighterAttributes;
    fCommentAttri :TSynHighlighterAttributes;
    fIdentifierAttri :TSynHighlighterAttributes;
    fKeyAttri :TSynHighlighterAttributes;
    fLaceAttri :TSynHighlighterAttributes;
    fOperatorAndSymbolsAttri :TSynHighlighterAttributes;
    fPredefinedAttri :TSynHighlighterAttributes;
    fResultValueAttri :TSynHighlighterAttributes;
    fSpaceAttri :TSynHighlighterAttributes;
    fStringAttri :TSynHighlighterAttributes;
    function KeyHash(ToHash :PChar) :Integer;
    function KeyComp(const aKey :string) :Boolean;
    function Func0 :TtkTokenKind;
    function Func15 :TtkTokenKind;
    function Func19 :TtkTokenKind;
    function Func20 :TtkTokenKind;
    function Func21 :TtkTokenKind;
    function Func23 :TtkTokenKind;
    function Func25 :TtkTokenKind;
    function Func28 :TtkTokenKind;
    function Func29 :TtkTokenKind;
    function Func30 :TtkTokenKind;
    function Func31 :TtkTokenKind;
    function Func33 :TtkTokenKind;
    function Func36 :TtkTokenKind;
    function Func37 :TtkTokenKind;
    function Func39 :TtkTokenKind;
    function Func41 :TtkTokenKind;
    function Func42 :TtkTokenKind;
    function Func43 :TtkTokenKind;
    function Func45 :TtkTokenKind;
    function Func47 :TtkTokenKind;
    function Func49 :TtkTokenKind;
    function Func50 :TtkTokenKind;
    function Func52 :TtkTokenKind;
    function Func54 :TtkTokenKind;
    function Func55 :TtkTokenKind;
    function Func56 :TtkTokenKind;
    function Func57 :TtkTokenKind;
    function Func58 :TtkTokenKind;
    function Func59 :TtkTokenKind;
    function Func62 :TtkTokenKind;
    function Func63 :TtkTokenKind;
    function Func64 :TtkTokenKind;
    function Func65 :TtkTokenKind;
    function Func66 :TtkTokenKind;
    function Func68 :TtkTokenKind;
    function Func69 :TtkTokenKind;
    function Func71 :TtkTokenKind;
    function Func73 :TtkTokenKind;
    function Func74 :TtkTokenKind;
    function Func75 :TtkTokenKind;
    function Func76 :TtkTokenKind;
    function Func77 :TtkTokenKind;
    function Func78 :TtkTokenKind;
    function Func82 :TtkTokenKind;
    function Func83 :TtkTokenKind;
    function Func84 :TtkTokenKind;
    function Func85 :TtkTokenKind;
    function Func86 :TtkTokenKind;
    function Func87 :TtkTokenKind;
    function Func89 :TtkTokenKind;
    function Func93 :TtkTokenKind;
    function Func95 :TtkTokenKind;
    function Func97 :TtkTokenKind;
    function Func98 :TtkTokenKind;
    function Func99 :TtkTokenKind;
    function Func101 :TtkTokenKind;
    function Func108 :TtkTokenKind;
    function Func113 :TtkTokenKind;
    function Func116 :TtkTokenKind;
    function Func120 :TtkTokenKind;
    function Func133 :TtkTokenKind;
    function Func144 :TtkTokenKind;
    procedure IdentProc;
    procedure OperatorAndSymbolProc;
    procedure UnknownProc;
    function AltFunc :TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe :PChar) :TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure EiffelCommentOpenProc;
    procedure EiffelCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
  protected
    function GetIdentChars :TSynIdentChars; override;
    function GetSampleSource :string; override;
    function IsFilterStored :Boolean; override;
  public
    constructor Create(AOwner :TComponent); override;
    class function GetLanguageName :string; override;
    function GetRange :Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value :Pointer); override;
    function GetDefaultAttribute(Index :integer) :TSynHighlighterAttributes; override;
    function GetEol :Boolean; override;
    function GetKeyWords :string;
    function GetTokenID :TtkTokenKind;
    procedure SetLine(NewValue :string; LineNumber :Integer); override;
    function GetToken :string; override;
    function GetTokenAttribute :TSynHighlighterAttributes; override;
    function GetTokenKind :integer; override;
    function GetTokenPos :Integer; override;
    procedure Next; override;
  published
    property BasicTypesAttri :TSynHighlighterAttributes read fBasicTypesAttri write fBasicTypesAttri;
    property CommentAttri :TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri :TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri :TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property LaceAttri :TSynHighlighterAttributes read fLaceAttri write fLaceAttri;
    property OperatorAndSymbolsAttri :TSynHighlighterAttributes read fOperatorAndSymbolsAttri write fOperatorAndSymbolsAttri;
    property PredefinedAttri :TSynHighlighterAttributes read fPredefinedAttri write fPredefinedAttri;
    property ResultValueAttri :TSynHighlighterAttributes read fResultValueAttri write fResultValueAttri;
    property SpaceAttri :TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri :TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  Identifiers :array[#0..#255] of ByteBool;
  mHashTable :array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J :Char;
begin
  for I := #0 to #255 do
    begin
      case I of
        '_', 'a'..'z', 'A'..'Z' :Identifiers[I] := True;
        else
          Identifiers[I] := False;
      end;
      J := UpCase(I);
      case I in ['_', 'A'..'Z', 'a'..'z'] of
        True :mHashTable[I] := Ord(J) - 64
        else
          mHashTable[I] := 0;
      end;
    end;
end;

procedure TSynEiffelSyn.InitIdent;
var
  I :Integer;
  pF :PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    begin
      pF^ := AltFunc;
      Inc(pF);
    end;
  fIdentFuncTable[0] := Func0;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[144] := Func144;
end;

function TSynEiffelSyn.KeyHash(ToHash :PChar) :Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
    begin
      inc(Result, mHashTable[ToHash^]);
      inc(ToHash);
    end;
  fStringLen := ToHash - fToIdent;
end;

function TSynEiffelSyn.KeyComp(const aKey :string) :Boolean;
var
  I :Integer;
  Temp :PChar;
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

function TSynEiffelSyn.Func0 :TtkTokenKind;
begin
  if KeyComp('@') then
    Result := tkOperatorAndSymbols
  else
    if KeyComp(']') then
      Result := tkOperatorAndSymbols
    else
      if KeyComp('*') then
        Result := tkOperatorAndSymbols
      else
        if KeyComp('&') then
          Result := tkOperatorAndSymbols
        else
          if KeyComp('|') then
            Result := tkOperatorAndSymbols
          else
            if KeyComp('#') then
              Result := tkOperatorAndSymbols
            else
              if KeyComp('.') then
                Result := tkOperatorAndSymbols
              else
                if KeyComp('=') then
                  Result := tkOperatorAndSymbols
                else
                  if KeyComp('(') then
                    Result := tkOperatorAndSymbols
                  else
                    if KeyComp('/') then
                      Result := tkOperatorAndSymbols
                    else
                      if KeyComp('[') then
                        Result := tkOperatorAndSymbols
                      else
                        if KeyComp('\\') then
                          Result := tkOperatorAndSymbols
                        else
                          if KeyComp('$') then
                            Result := tkOperatorAndSymbols
                          else
                            if KeyComp('<') then
                              Result := tkOperatorAndSymbols
                            else
                              if KeyComp(':=') then
                                Result := tkOperatorAndSymbols
                              else
                                if KeyComp('^') then
                                  Result := tkOperatorAndSymbols
                                else
                                  if KeyComp('!') then
                                    Result := tkOperatorAndSymbols
                                  else
                                    if KeyComp('<>') then
                                      Result := tkOperatorAndSymbols
                                    else
                                      if KeyComp('>') then
                                        Result := tkOperatorAndSymbols
                                      else
                                        if KeyComp('+') then
                                          Result := tkOperatorAndSymbols
                                        else
                                          if KeyComp('/=') then
                                            Result := tkOperatorAndSymbols
                                          else
                                            if KeyComp('//') then
                                              Result := tkOperatorAndSymbols
                                            else
                                              if KeyComp(':') then
                                                Result := tkOperatorAndSymbols
                                              else
                                                if KeyComp(';') then
                                                  Result := tkOperatorAndSymbols
                                                else
                                                  if KeyComp('-') then
                                                    Result := tkOperatorAndSymbols
                                                  else
                                                    if KeyComp(')') then
                                                      Result := tkOperatorAndSymbols
                                                    else
                                                      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func15 :TtkTokenKind;
begin
  if KeyComp('if') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func19 :TtkTokenKind;
begin
  if KeyComp('do') then
    Result := tkKey
  else
    if KeyComp('and') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func20 :TtkTokenKind;
begin
  if KeyComp('as') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func21 :TtkTokenKind;
begin
  if KeyComp('%U') then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func23 :TtkTokenKind;
begin
  if KeyComp('end') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func25 :TtkTokenKind;
begin
  if KeyComp('all') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func28 :TtkTokenKind;
begin
  if KeyComp('is') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func29 :TtkTokenKind;
begin
  if KeyComp('no') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func30 :TtkTokenKind;
begin
  if KeyComp('check') then
    Result := tkKey
  else
    if KeyComp('make') then
      Result := tkLace
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func31 :TtkTokenKind;
begin
  if KeyComp('old') then
    Result := tkKey
  else
    if KeyComp('BIT') then
      Result := tkPredefined
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func33 :TtkTokenKind;
begin
  if KeyComp('or') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func36 :TtkTokenKind;
begin
  if KeyComp('real') then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func37 :TtkTokenKind;
begin
  if KeyComp('like') then
    Result := tkKey
  else
    if KeyComp('once') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func39 :TtkTokenKind;
begin
  if KeyComp('debug') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func41 :TtkTokenKind;
begin
  if KeyComp('else') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func42 :TtkTokenKind;
begin
  if KeyComp('alias') then
    Result := tkKey
  else
    if KeyComp('adapt') then
      Result := tkLace
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func43 :TtkTokenKind;
begin
  if KeyComp('local') then
    Result := tkKey
  else
    if KeyComp('false') then
      Result := tkPredefined
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func45 :TtkTokenKind;
begin
  if KeyComp('comma') then
    Result := tkLace
  else
    if KeyComp('use') then
      Result := tkLace
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func47 :TtkTokenKind;
begin
  if KeyComp('trace') then
    Result := tkLace
  else
    if KeyComp('then') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func49 :TtkTokenKind;
begin
  if KeyComp('yes') then
    Result := tkLace
  else
    if KeyComp('not') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func50 :TtkTokenKind;
begin
  if KeyComp('void') then
    Result := tkPredefined
  else
    if KeyComp('when') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func52 :TtkTokenKind;
begin
  if KeyComp('from') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func54 :TtkTokenKind;
begin
  if KeyComp('class') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func55 :TtkTokenKind;
begin
  if KeyComp('object') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func56 :TtkTokenKind;
begin
  if KeyComp('rename') then
    Result := tkKey
  else
    if KeyComp('elseif') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func57 :TtkTokenKind;
begin
  if KeyComp('xor') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func58 :TtkTokenKind;
begin
  if KeyComp('loop') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func59 :TtkTokenKind;
begin
  if KeyComp('double') then
    Result := tkBasicTypes
  else
    if KeyComp('colon') then
      Result := tkLace
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func62 :TtkTokenKind;
begin
  if KeyComp('infix') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func63 :TtkTokenKind;
begin
  if KeyComp('Array') then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func64 :TtkTokenKind;
begin
  if KeyComp('boolean') then
    Result := tkBasicTypes
  else
    if KeyComp('true') then
      Result := tkPredefined
    else
      if KeyComp('select') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

function TSynEiffelSyn.Func65 :TtkTokenKind;
begin
  if KeyComp('deferred') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func66 :TtkTokenKind;
begin
  if KeyComp('redefine') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func68 :TtkTokenKind;
begin
  if KeyComp('include') then
    Result := tkLace
  else
    if KeyComp('root') then
      Result := tkLace
    else
      if KeyComp('ignore') then
        Result := tkLace
      else
        Result := tkIdentifier;
end;

function TSynEiffelSyn.Func69 :TtkTokenKind;
begin
  if KeyComp('default') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func71 :TtkTokenKind;
begin
  if KeyComp('rescue') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func73 :TtkTokenKind;
begin
  if KeyComp('expanded') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func74 :TtkTokenKind;
begin
  if KeyComp('exclude') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func75 :TtkTokenKind;
begin
  if KeyComp('generate') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func76 :TtkTokenKind;
begin
  if KeyComp('until') then
    Result := tkKey
  else
    if KeyComp('feature') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func77 :TtkTokenKind;
begin
  if KeyComp('character') then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func78 :TtkTokenKind;
begin
  if KeyComp('undefine') then
    Result := tkKey
  else
    if KeyComp('prefix') then
      Result := tkKey
    else
      if KeyComp('integer') then
        Result := tkBasicTypes
      else
        if KeyComp('visible') then
          Result := tkLace
        else
          Result := tkIdentifier;
end;

function TSynEiffelSyn.Func82 :TtkTokenKind;
begin
  if KeyComp('ensure') then
    Result := tkKey
  else
    if KeyComp('strip') then
      Result := tkPredefined
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func83 :TtkTokenKind;
begin
  if KeyComp('inherit') then
    Result := tkKey
  else
    if KeyComp('implies') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func84 :TtkTokenKind;
begin
  if KeyComp('frozen') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func85 :TtkTokenKind;
begin
  if KeyComp('variant') then
    Result := tkKey
  else
    if KeyComp('creation') then
      Result := tkKey
    else
      if KeyComp('separate') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

function TSynEiffelSyn.Func86 :TtkTokenKind;
begin
  if KeyComp('retry') then
    Result := tkKey
  else
    if KeyComp('inspect') then
      Result := tkKey
    else
      if KeyComp('indexing') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

function TSynEiffelSyn.Func87 :TtkTokenKind;
begin
  if KeyComp('string') then
    Result := tkBasicTypes
  else
    if KeyComp('unique') then
      Result := tkPredefined
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func89 :TtkTokenKind;
begin
  if KeyComp('option') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func93 :TtkTokenKind;
begin
  if KeyComp('require') then
    Result := tkKey
  else
    if KeyComp('obsolete') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynEiffelSyn.Func95 :TtkTokenKind;
begin
  if KeyComp('result') then
    Result := tkResultValue
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func97 :TtkTokenKind;
begin
  if KeyComp('pointer') then
    Result := tkBasicTypes
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func98 :TtkTokenKind;
begin
  if KeyComp('cluster') then
    Result := tkLace
  else
    if KeyComp('executable') then
      Result := tkLace
    else
      if KeyComp('export') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

function TSynEiffelSyn.Func99 :TtkTokenKind;
begin
  if KeyComp('external') then
    Result := tkKey
  else
    if KeyComp('current') then
      Result := tkPredefined
    else
      if KeyComp('identifier') then
        Result := tkLace
      else
        Result := tkIdentifier;
end;

function TSynEiffelSyn.Func101 :TtkTokenKind;
begin
  if KeyComp('system') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func108 :TtkTokenKind;
begin
  if KeyComp('invariant') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func113 :TtkTokenKind;
begin
  if KeyComp('optimize') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func116 :TtkTokenKind;
begin
  if KeyComp('precompiled') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func120 :TtkTokenKind;
begin
  if KeyComp('assertion') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func133 :TtkTokenKind;
begin
  if KeyComp('precursor') then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.Func144 :TtkTokenKind;
begin
  if KeyComp('include_path') then
    Result := tkLace
  else
    Result := tkIdentifier;
end;

function TSynEiffelSyn.AltFunc :TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynEiffelSyn.IdentKind(MayBe :PChar) :TtkTokenKind;
var
  HashKey :Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynEiffelSyn.MakeMethodTables;
var
  I :Char;
begin
  for I := #0 to #255 do
    case I of
      #33, #35..#44, #46..#47, #58..#64, #91..#96, #123..#127:
        fProcTable[I] := OperatorAndSymbolProc;
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '-': fProcTable[I] := EiffelCommentOpenProc;
      '"': fProcTable[I] := StringOpenProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      'A'..'Z', 'a'..'z': fProcTable[I] := IdentProc;
      else
        fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynEiffelSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynEiffelSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynEiffelSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynEiffelSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynEiffelSyn.OperatorAndSymbolProc;
begin
  fTokenID := tkIdentifier;
  if fLine[Run] = #33 then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if fLine[Run] in [#35..#44] then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if fLine[Run] in [#46..#47] then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if fLine[Run] in [#58..#64] then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if fLine[Run] in [#91..#96] then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
  if fLine[Run] in [#123..#127] then
    begin
      fRange := rsOperatorAndSymbolProc;
      fTokenID := tkOperatorAndSymbols;
      Inc(Run);
      Exit;
    end;
end;

procedure TSynEiffelSyn.EiffelCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run - 1] = '-') and (fLine[Run] = '-') then
    begin
      fRange := rsEiffelComment;
      EiffelCommentProc;
      fTokenID := tkComment;
    end
  else
    fTokenID := tkOperatorAndSymbols;
end;

procedure TSynEiffelSyn.EiffelCommentProc;
begin
  fTokenID := tkComment;
  repeat
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynEiffelSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynEiffelSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '"') then
      begin
        Inc(Run, 1);
        fRange := rsUnKnown;
        Break;
      end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

constructor TSynEiffelSyn.Create(AOwner :TComponent);
begin
  inherited Create(AOwner);
  fBasicTypesAttri := TSynHighLighterAttributes.Create(SYNS_AttrBasicTypes);
  fBasicTypesAttri.Style := [fsBold];
  fBasicTypesAttri.Foreground := clBlue;
  AddAttribute(fBasicTypesAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clTeal;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clMaroon;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fLaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrLace);
  fLaceAttri.Style := [fsBold];
  fLaceAttri.Foreground := clNavy;
  AddAttribute(fLaceAttri);

  fOperatorAndSymbolsAttri := TSynHighLighterAttributes.Create(SYNS_AttrOperatorAndSymbols);
  fOperatorAndSymbolsAttri.Style := [fsBold];
  fOperatorAndSymbolsAttri.Foreground := clOlive;
  AddAttribute(fOperatorAndSymbolsAttri);

  fPredefinedAttri := TSynHighLighterAttributes.Create(SYNS_AttrPredefined);
  fPredefinedAttri.Style := [fsBold];
  fPredefinedAttri.Foreground := clRed;
  AddAttribute(fPredefinedAttri);

  fResultValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrResultValue);
  fResultValueAttri.Style := [fsBold];
  fResultValueAttri.Foreground := clPurple;
  AddAttribute(fResultValueAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Style := [fsItalic];
  fStringAttri.Foreground := clGray;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterEiffel;
  fRange := rsUnknown;
end;

procedure TSynEiffelSyn.SetLine(NewValue :string; LineNumber :Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynEiffelSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynEiffelSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
    inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynEiffelSyn.Next;
begin
  fTokenPos := Run;
  fRange := rsUnknown;
  fProcTable[fLine[Run]];
end;

function TSynEiffelSyn.GetDefaultAttribute(Index :integer) :TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT :Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER :Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD :Result := fKeyAttri;
    SYN_ATTR_STRING :Result := fStringAttri;
    SYN_ATTR_WHITESPACE :Result := fSpaceAttri;
    else
      Result := nil;
  end;
end;

function TSynEiffelSyn.GetEol :Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynEiffelSyn.GetKeyWords :string;
begin
  Result :=
    '-,!,#,$,%U,&,(,),*,.,/,//,/=,:,:=,;,@,[,\\,],^,|,+,<,<>,=,>,adapt,ali' +
    'as,all,and,Array,as,assertion,BIT,boolean,character,check,class,cluste' +
    'r,colon,comma,creation,current,debug,default,deferred,do,double,else,e' +
    'lseif,end,ensure,exclude,executable,expanded,export,external,false,fea' +
    'ture,from,frozen,generate,identifier,if,ignore,implies,include,include' +
    '_path,indexing,infix,inherit,inspect,integer,invariant,is,like,local,l' +
    'oop,make,no,not,object,obsolete,old,once,optimize,option,or,pointer,pr' +
    'ecompiled,precursor,prefix,real,redefine,rename,require,rescue,result,' +
    'retry,root,select,separate,string,strip,system,then,trace,true,undefin' +
    'e,unique,until,use,variant,visible,void,when,xor,yes';
end;

function TSynEiffelSyn.GetToken :string;
var
  Len :LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynEiffelSyn.GetTokenID :TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynEiffelSyn.GetTokenAttribute :TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkBasicTypes :Result := fBasicTypesAttri;
    tkComment :Result := fCommentAttri;
    tkIdentifier :Result := fIdentifierAttri;
    tkKey :Result := fKeyAttri;
    tkLace :Result := fLaceAttri;
    tkOperatorAndSymbols :Result := fOperatorAndSymbolsAttri;
    tkPredefined :Result := fPredefinedAttri;
    tkResultValue :Result := fResultValueAttri;
    tkSpace :Result := fSpaceAttri;
    tkString :Result := fStringAttri;
    tkUnknown :Result := fIdentifierAttri;
    else
      Result := nil;
  end;
end;

function TSynEiffelSyn.GetTokenKind :integer;
begin
  Result := Ord(fTokenId);
end;

function TSynEiffelSyn.GetTokenPos :Integer;
begin
  Result := fTokenPos;
end;

function TSynEiffelSyn.GetIdentChars :TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z'];
end;

function TSynEiffelSyn.GetSampleSource :string;
begin
  Result := '-- Eiffel sample source from SmartEiffel'#13#10 +
    'class FIBONACCI'#13#10 +
    '-- Eiffel comment'#13#10 +
    'creation make'#13#10 +
    #13#10 +
    'feature'#13#10 +
    #13#10 +
    '   make is'#13#10 +
    '      do'#13#10 +
    '         if argument_count /= 1 or else'#13#10 +
    '            not argument(1).is_integer'#13#10 +
    '          then'#13#10 +
    '            io.put_string("Usage: ");'#13#10 +
    '            io.put_string(argument(0));'#13#10 +
    '            io.put_string(" <Integer_value>%N");'#13#10 +
    '            die_with_code(exit_failure_code);'#13#10 +
    '         end;'#13#10 +
    '         io.put_integer(fibonacci(argument(1).to_integer));'#13#10 +
    '         io.put_new_line;'#13#10 +
    '      end;'#13#10 +
    '   -- Eiffel comment'#13#10 +
    '   fibonacci(i: INTEGER): INTEGER is'#13#10 +
    '      require -- Eiffel comment'#13#10 +
    '         i >= 0'#13#10 +
    '      do'#13#10 +
    '         if i = 0 then'#13#10 +
    '            Result := 1;'#13#10 +
    '         elseif i = 1 then'#13#10 +
    '            Result := 1;'#13#10 +
    '         else'#13#10 +
    '            Result := fibonacci(i - 1) + fibonacci(i - 2) ;'#13#10 +
    '         end;'#13#10 +
    '      end;'#13#10 +
    #13#10 +
    'end';
end;

function TSynEiffelSyn.IsFilterStored :Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterEiffel;
end;

class function TSynEiffelSyn.GetLanguageName :string;
begin
  Result := SYNS_LangEiffel;
end;

procedure TSynEiffelSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynEiffelSyn.SetRange(Value :Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynEiffelSyn.GetRange :Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynEiffelSyn);
{$ENDIF}
end.
