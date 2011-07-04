{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVrml.pas, released 2002-10-21.
The Original Code is based on: SynHighlighterJScript.pas, released 2000-04-14.
The Original Code is based on the mwJScript.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony de Buys.
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

$Id: SynHighlighterVrml97.pas,v 1.6 2004/07/31 16:20:08 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Vrml97/X3D/JavaScript highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it)
@created(november 2002 [December 1999, converted to SynEdit April 14, 2000])
@lastmod(2002-11-03)
The SynHighlighterVrml97 unit provides SynEdit with a Vrml97/X3D/JavaScript (.wrl;*.x3d) highlighter.
The highlighter formats Vrml97/X3D source code highlighting keywords, strings, numbers and characters.
}

{$IFNDEF QSYNHIGHLIGHTERVRML97}
unit SynHighlighterVrml97;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QControls,
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Messages,
  Registry,
  Controls,
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
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown,
    tkNonReservedKey,
    tkEvent,
    tkVrmlAppearance,
    tkVrmlAttribute,
    tkVrmlDefinition,
    tkVrmlEvent,
    tkVrmlGrouping,
    tkVrmlInterpolator,
    tkVrmlLight,
    tkVrmlNode,
    tkVrmlParameter,
    tkVrmlproto,
    tkVrmlSensor,
    tkVrmlShape,
    tkVrmlShape_Hint,
    tkVrmlTime_dependent,
    tkVrmlViewpoint,
    tkVrmlWorldInfo,
    tkX3DDocType,
    tkX3DHeader);

  TRangeState = (rsNormalText, rsComment, rsX3DHeader, rsX3DDocType);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function :TtkTokenKind of object;

type
  TSynVrml97Syn = class(TSynCustomHighLighter)
  private
    fRange :TRangeState;
    isDoctype :boolean;
    fLine :PChar;
    fLineNumber :Integer;
    fProcTable :array[#0..#255] of TProcTableProc;
    Run :LongInt;
    fStringLen :Integer;
    fToIdent :PChar;
    fTokenPos :Integer;
    FTokenID :TtkTokenKind;
    fIdentFuncTable :array[0..354] of TIdentFuncTableFunc;
    fCommentAttri :TSynHighlighterAttributes;
    fIdentifierAttri :TSynHighlighterAttributes;
    fKeyAttri :TSynHighlighterAttributes;
    fNonReservedKeyAttri :TSynHighlighterAttributes;
    fEventAttri :TSynHighlighterAttributes;
    fNumberAttri :TSynHighlighterAttributes;
    fSpaceAttri :TSynHighlighterAttributes;
    fStringAttri :TSynHighlighterAttributes;
    fSymbolAttri :TSynHighlighterAttributes;

    fVrmlAppearanceAttri :TSynHighlighterAttributes;
    fVrmlAttributeAttri :TSynHighlighterAttributes;
    fVrmlDefinitionAttri :TSynHighlighterAttributes;
    fVrmlEventAttri :TSynHighlighterAttributes;
    fVrmlGroupingAttri :TSynHighlighterAttributes;
    fVrmlInterpolatorAttri :TSynHighlighterAttributes;
    fVrmlLightAttri :TSynHighlighterAttributes;
    fVrmlNodeAttri :TSynHighlighterAttributes;
    fVrmlParameterAttri :TSynHighlighterAttributes;
    fVrmlprotoAttri :TSynHighlighterAttributes;
    fVrmlSensorAttri :TSynHighlighterAttributes;
    fVrmlShapeAttri :TSynHighlighterAttributes;
    fVrmlShape_HintAttri :TSynHighlighterAttributes;
    fVrmlTime_dependentAttri :TSynHighlighterAttributes;
    fVrmlViewpointAttri :TSynHighlighterAttributes;
    fVrmlWorldInfoAttri :TSynHighlighterAttributes;
    fX3DDocTypeAttri :TSynHighlighterAttributes;
    fX3DHeaderAttri :TSynHighlighterAttributes;

    function KeyHash(ToHash :PChar) :Integer;
    function KeyComp(const aKey :string) :Boolean;
    function Func5 :TtkTokenKind;
    function Func15 :TtkTokenKind;
    function Func17 :TtkTokenKind;
    function Func18 :TtkTokenKind;
    function Func19 :TtkTokenKind;
    function Func22 :TtkTokenKind;
    function Func23 :TtkTokenKind;
    function Func25 :TtkTokenKind;
    function Func26 :TtkTokenKind;
    function Func27 :TtkTokenKind;
    function Func28 :TtkTokenKind;
    function Func29 :TtkTokenKind;
    function Func30 :TtkTokenKind;
    function Func31 :TtkTokenKind;
    function Func32 :TtkTokenKind;
    function Func33 :TtkTokenKind;
    function Func34 :TtkTokenKind;
    function Func35 :TtkTokenKind;
    function Func36 :TtkTokenKind;
    function Func37 :TtkTokenKind;
    function Func38 :TtkTokenKind;
    function Func39 :TtkTokenKind;
    function Func40 :TtkTokenKind;
    function Func41 :TtkTokenKind;
    function Func42 :TtkTokenKind;
    function Func43 :TtkTokenKind;
    function Func44 :TtkTokenKind;
    function Func45 :TtkTokenKind;
    function Func46 :TtkTokenKind;
    function Func47 :TtkTokenKind;
    function Func48 :TtkTokenKind;
    function Func49 :TtkTokenKind;
    function Func50 :TtkTokenKind;
    function Func51 :TtkTokenKind;
    function Func52 :TtkTokenKind;
    function Func53 :TtkTokenKind;
    function Func54 :TtkTokenKind;
    function Func55 :TtkTokenKind;
    function Func56 :TtkTokenKind;
    function Func57 :TtkTokenKind;
    function Func58 :TtkTokenKind;
    function Func59 :TtkTokenKind;
    function Func60 :TtkTokenKind;
    function Func61 :TtkTokenKind;
    function Func62 :TtkTokenKind;
    function Func63 :TtkTokenKind;
    function Func64 :TtkTokenKind;
    function Func65 :TtkTokenKind;
    function Func66 :TtkTokenKind;
    function Func67 :TtkTokenKind;
    function Func68 :TtkTokenKind;
    function Func69 :TtkTokenKind;
    function Func70 :TtkTokenKind;
    function Func71 :TtkTokenKind;
    function Func72 :TtkTokenKind;
    function Func73 :TtkTokenKind;
    function Func74 :TtkTokenKind;
    function Func75 :TtkTokenKind;
    function Func76 :TtkTokenKind;
    function Func77 :TtkTokenKind;
    function Func78 :TtkTokenKind;
    function Func79 :TtkTokenKind;
    function Func80 :TtkTokenKind;
    function Func81 :TtkTokenKind;
    function Func82 :TtkTokenKind;
    function Func83 :TtkTokenKind;
    function Func84 :TtkTokenKind;
    function Func85 :TtkTokenKind;
    function Func86 :TtkTokenKind;
    function Func87 :TtkTokenKind;
    function Func88 :TtkTokenKind;
    function Func89 :TtkTokenKind;
    function Func90 :TtkTokenKind;
    function Func91 :TtkTokenKind;
    function Func92 :TtkTokenKind;
    function Func93 :TtkTokenKind;
    function Func94 :TtkTokenKind;
    function Func95 :TtkTokenKind;
    function Func96 :TtkTokenKind;
    function Func97 :TtkTokenKind;
    function Func98 :TtkTokenKind;
    function Func99 :TtkTokenKind;
    function Func100 :TtkTokenKind;
    function Func101 :TtkTokenKind;
    function Func102 :TtkTokenKind;
    function Func103 :TtkTokenKind;
    function Func104 :TtkTokenKind;
    function Func105 :TtkTokenKind;
    function Func106 :TtkTokenKind;
    function Func107 :TtkTokenKind;
    function Func108 :TtkTokenKind;
    function Func109 :TtkTokenKind;
    function Func110 :TtkTokenKind;
    function Func111 :TtkTokenKind;
    function Func112 :TtkTokenKind;
    function Func113 :TtkTokenKind;
    function Func114 :TtkTokenKind;
    function Func115 :TtkTokenKind;
    function Func116 :TtkTokenKind;
    function Func117 :TtkTokenKind;
    function Func118 :TtkTokenKind;
    function Func119 :TtkTokenKind;
    function Func120 :TtkTokenKind;
    function Func121 :TtkTokenKind;
    function Func122 :TtkTokenKind;
    function Func123 :TtkTokenKind;
    function Func124 :TtkTokenKind;
    function Func125 :TtkTokenKind;
    function Func126 :TtkTokenKind;
    function Func128 :TtkTokenKind;
    function Func129 :TtkTokenKind;
    function Func130 :TtkTokenKind;
    function Func131 :TtkTokenKind;
    function Func132 :TtkTokenKind;
    function Func133 :TtkTokenKind;
    function Func134 :TtkTokenKind;
    function Func135 :TtkTokenKind;
    function Func136 :TtkTokenKind;
    function Func137 :TtkTokenKind;
    function Func138 :TtkTokenKind;
    function Func139 :TtkTokenKind;
    function Func140 :TtkTokenKind;
    function Func141 :TtkTokenKind;
    function Func142 :TtkTokenKind;
    function Func143 :TtkTokenKind;
    function Func144 :TtkTokenKind;
    function Func145 :TtkTokenKind;
    function Func146 :TtkTokenKind;
    function Func147 :TtkTokenKind;
    function Func148 :TtkTokenKind;
    function Func149 :TtkTokenKind;
    function Func150 :TtkTokenKind;
    function Func151 :TtkTokenKind;
    function Func153 :TtkTokenKind;
    function Func154 :TtkTokenKind;
    function Func155 :TtkTokenKind;
    function Func156 :TtkTokenKind;
    function Func157 :TtkTokenKind;
    function Func158 :TtkTokenKind;
    function Func159 :TtkTokenKind;
    function Func160 :TtkTokenKind;
    function Func161 :TtkTokenKind;
    function Func162 :TtkTokenKind;
    function Func164 :TtkTokenKind;
    function Func166 :TtkTokenKind;
    function Func167 :TtkTokenKind;
    function Func168 :TtkTokenKind;
    function Func169 :TtkTokenKind;
    function Func170 :TtkTokenKind;
    function Func171 :TtkTokenKind;
    function Func172 :TtkTokenKind;
    function Func173 :TtkTokenKind;
    function Func174 :TtkTokenKind;
    function Func175 :TtkTokenKind;
    function Func176 :TtkTokenKind;
    function Func177 :TtkTokenKind;
    function Func178 :TtkTokenKind;
    function Func179 :TtkTokenKind;
    function Func180 :TtkTokenKind;
    function Func181 :TtkTokenKind;
    function Func182 :TtkTokenKind;
    function Func183 :TtkTokenKind;
    function Func184 :TtkTokenKind;
    function Func185 :TtkTokenKind;
    function Func186 :TtkTokenKind;
    function Func188 :TtkTokenKind;
    function Func189 :TtkTokenKind;
    function Func190 :TtkTokenKind;
    function Func194 :TtkTokenKind;
    function Func196 :TtkTokenKind;
    function Func198 :TtkTokenKind;
    function Func199 :TtkTokenKind;
    function Func200 :TtkTokenKind;
    function Func202 :TtkTokenKind;
    function Func204 :TtkTokenKind;
    function Func206 :TtkTokenKind;
    function Func209 :TtkTokenKind;
    function Func210 :TtkTokenKind;
    function Func212 :TtkTokenKind;
    function Func213 :TtkTokenKind;
    function Func214 :TtkTokenKind;
    function Func215 :TtkTokenKind;
    function Func216 :TtkTokenKind;
    function Func217 :TtkTokenKind;
    function Func218 :TtkTokenKind;
    function Func220 :TtkTokenKind;
    function Func221 :TtkTokenKind;
    function Func222 :TtkTokenKind;
    function Func223 :TtkTokenKind;
    function Func226 :TtkTokenKind;
    function Func229 :TtkTokenKind;
    function Func233 :TtkTokenKind;
    function Func234 :TtkTokenKind;
    function Func235 :TtkTokenKind;
    function Func236 :TtkTokenKind;
    function Func237 :TtkTokenKind;
    function Func239 :TtkTokenKind;
    function Func245 :TtkTokenKind;
    function Func252 :TtkTokenKind;
    function Func255 :TtkTokenKind;
    function Func260 :TtkTokenKind;
    function Func262 :TtkTokenKind;
    function Func263 :TtkTokenKind;
    function Func264 :TtkTokenKind;
    function Func267 :TtkTokenKind;
    function Func271 :TtkTokenKind;
    function Func278 :TtkTokenKind;
    function Func280 :TtkTokenKind;
    function Func283 :TtkTokenKind;
    function Func284 :TtkTokenKind;
    function Func289 :TtkTokenKind;
    function Func292 :TtkTokenKind;
    function Func303 :TtkTokenKind;
    function Func307 :TtkTokenKind;
    function Func308 :TtkTokenKind;
    function Func318 :TtkTokenKind;
    function Func320 :TtkTokenKind;
    function Func354 :TtkTokenKind;
    procedure AndSymbolProc;
    procedure CommentProc;
    procedure DiesisCommentProc;
    procedure X3DDocTypeOpenProc;
    procedure X3DDocTypeProc;
    procedure X3DHeaderOpenProc;
    procedure X3DHeaderProc;
    procedure InCommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function AltFunc :TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe :PChar) :TtkTokenKind;
    procedure MakeMethodTables;
    function NextTokenIs(T :string) :Boolean;
  protected
    function GetIdentChars :TSynIdentChars; override;
    function GetSampleSource :string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName :string; override;
  public
    constructor Create(AOwner :TComponent); override;
    function GetDefaultAttribute(Index :integer) :TSynHighlighterAttributes;
      override;
    function GetEol :Boolean; override;
    function GetRange :Pointer; override;
    function GetTokenID :TtkTokenKind;
    procedure SetLine(NewValue :string; LineNumber :Integer); override;
    function GetToken :string; override;
    function GetTokenAttribute :TSynHighlighterAttributes; override;
    function GetTokenKind :integer; override;
    function GetTokenPos :Integer; override;
    procedure Next; override;
    procedure SetRange(Value :Pointer); override;
    procedure ResetRange; override;
  published
    property NonReservedKeyAttri :TSynHighlighterAttributes read fNonReservedKeyAttri write fNonReservedKeyAttri;
    property NumberAttri :TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri :TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri :TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri :TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property CommentAttri :TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri :TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property EcmaScriptKeyAttri :TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property EcmaScriptEventAttri :TSynHighlighterAttributes read fEventAttri write fEventAttri;

    property VrmlAppearanceAttri :TSynHighlighterAttributes read fVrmlAppearanceAttri write fVrmlAppearanceAttri;
    property VrmlAttributeAttri :TSynHighlighterAttributes read fVrmlAttributeAttri write fVrmlAttributeAttri;
    property VrmlDefinitionAttri :TSynHighlighterAttributes read fVrmlDefinitionAttri write fVrmlDefinitionAttri;
    property VrmlEventAttri :TSynHighlighterAttributes read fVrmlEventAttri write fVrmlEventAttri;
    property VrmlGroupingAttri :TSynHighlighterAttributes read fVrmlGroupingAttri write fVrmlGroupingAttri;
    property VrmlInterpolatorAttri :TSynHighlighterAttributes read fVrmlInterpolatorAttri write fVrmlInterpolatorAttri;
    property VrmlLightAttri :TSynHighlighterAttributes read fVrmlLightAttri write fVrmlLightAttri;
    property VrmlNodeAttri :TSynHighlighterAttributes read fVrmlNodeAttri write fVrmlNodeAttri;
    property VrmlParameterAttri :TSynHighlighterAttributes read fVrmlParameterAttri write fVrmlParameterAttri;
    property VrmlprotoAttri :TSynHighlighterAttributes read fVrmlprotoAttri write fVrmlprotoAttri;
    property VrmlSensorAttri :TSynHighlighterAttributes read fVrmlSensorAttri write fVrmlSensorAttri;
    property VrmlShapeAttri :TSynHighlighterAttributes read fVrmlShapeAttri write fVrmlShapeAttri;
    property VrmlShape_HintAttri :TSynHighlighterAttributes read fVrmlShape_HintAttri write fVrmlShape_HintAttri;
    property VrmlTime_dependentAttri :TSynHighlighterAttributes read fVrmlTime_dependentAttri write fVrmlTime_dependentAttri;
    property VrmlViewpointAttri :TSynHighlighterAttributes read fVrmlViewpointAttri write fVrmlViewpointAttri;
    property VrmlWorldInfoAttri :TSynHighlighterAttributes read fVrmlWorldInfoAttri write fVrmlWorldInfoAttri;
    property X3DDocTypeAttri :TSynHighlighterAttributes read fX3DDocTypeAttri write fX3DDocTypeAttri;
    property X3DHeaderAttri :TSynHighlighterAttributes read fX3DHeaderAttri write fX3DHeaderAttri;
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
        '_', '0'..'9', 'a'..'z', 'A'..'Z' :Identifiers[I] := True;
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

procedure TSynVrml97Syn.InitIdent;
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
  fIdentFuncTable[5] := Func5;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[115] := Func115;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[132] := Func132;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[134] := Func134;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[137] := Func137;
  fIdentFuncTable[138] := Func138;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[140] := Func140;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[142] := Func142;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[145] := Func145;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[147] := Func147;
  fIdentFuncTable[148] := Func148;
  fIdentFuncTable[149] := Func149;
  fIdentFuncTable[150] := Func150;
  fIdentFuncTable[151] := Func151;
  fIdentFuncTable[153] := Func153;
  fIdentFuncTable[154] := Func154;
  fIdentFuncTable[155] := Func155;
  fIdentFuncTable[156] := Func156;
  fIdentFuncTable[157] := Func157;
  fIdentFuncTable[158] := Func158;
  fIdentFuncTable[159] := Func159;
  fIdentFuncTable[160] := Func160;
  fIdentFuncTable[161] := Func161;
  fIdentFuncTable[162] := Func162;
  fIdentFuncTable[164] := Func164;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[167] := Func167;
  fIdentFuncTable[168] := Func168;
  fIdentFuncTable[169] := Func169;
  fIdentFuncTable[170] := Func170;
  fIdentFuncTable[171] := Func171;
  fIdentFuncTable[172] := Func172;
  fIdentFuncTable[173] := Func173;
  fIdentFuncTable[174] := Func174;
  fIdentFuncTable[175] := Func175;
  fIdentFuncTable[176] := Func176;
  fIdentFuncTable[177] := Func177;
  fIdentFuncTable[178] := Func178;
  fIdentFuncTable[179] := Func179;
  fIdentFuncTable[180] := Func180;
  fIdentFuncTable[181] := Func181;
  fIdentFuncTable[182] := Func182;
  fIdentFuncTable[183] := Func183;
  fIdentFuncTable[184] := Func184;
  fIdentFuncTable[185] := Func185;
  fIdentFuncTable[186] := Func186;
  fIdentFuncTable[188] := Func188;
  fIdentFuncTable[189] := Func189;
  fIdentFuncTable[190] := Func190;
  fIdentFuncTable[194] := Func194;
  fIdentFuncTable[196] := Func196;
  fIdentFuncTable[198] := Func198;
  fIdentFuncTable[199] := Func199;
  fIdentFuncTable[200] := Func200;
  fIdentFuncTable[202] := Func202;
  fIdentFuncTable[204] := Func204;
  fIdentFuncTable[206] := Func206;
  fIdentFuncTable[209] := Func209;
  fIdentFuncTable[210] := Func210;
  fIdentFuncTable[212] := Func212;
  fIdentFuncTable[213] := Func213;
  fIdentFuncTable[214] := Func214;
  fIdentFuncTable[215] := Func215;
  fIdentFuncTable[216] := Func216;
  fIdentFuncTable[217] := Func217;
  fIdentFuncTable[218] := Func218;
  fIdentFuncTable[220] := Func220;
  fIdentFuncTable[221] := Func221;
  fIdentFuncTable[222] := Func222;
  fIdentFuncTable[223] := Func223;
  fIdentFuncTable[226] := Func226;
  fIdentFuncTable[229] := Func229;
  fIdentFuncTable[233] := Func233;
  fIdentFuncTable[234] := Func234;
  fIdentFuncTable[235] := Func235;
  fIdentFuncTable[236] := Func236;
  fIdentFuncTable[237] := Func237;
  fIdentFuncTable[239] := Func239;
  fIdentFuncTable[245] := Func245;
  fIdentFuncTable[252] := Func252;
  fIdentFuncTable[255] := Func255;
  fIdentFuncTable[260] := Func260;
  fIdentFuncTable[262] := Func262;
  fIdentFuncTable[263] := Func263;
  fIdentFuncTable[264] := Func264;
  fIdentFuncTable[267] := Func267;
  fIdentFuncTable[271] := Func271;
  fIdentFuncTable[278] := Func278;
  fIdentFuncTable[280] := Func280;
  fIdentFuncTable[283] := Func283;
  fIdentFuncTable[284] := Func284;
  fIdentFuncTable[289] := Func289;
  fIdentFuncTable[292] := Func292;
  fIdentFuncTable[303] := Func303;
  fIdentFuncTable[307] := Func307;
  fIdentFuncTable[308] := Func308;
  fIdentFuncTable[318] := Func318;
  fIdentFuncTable[320] := Func320;
  fIdentFuncTable[354] := Func354;
end;

function TSynVrml97Syn.KeyHash(ToHash :PChar) :Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
    begin
      inc(Result, mHashTable[ToHash^]);
      inc(ToHash);
    end;
  fStringLen := ToHash - fToIdent;
end;

function TSynVrml97Syn.KeyComp(const aKey :string) :Boolean;
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
end;

//------------------------------------------------------------------------------

function TSynVrml97Syn.Func5 :TtkTokenKind;
begin
  if KeyComp('E') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func15 :TtkTokenKind;
begin
  if KeyComp('DEF') then
    Result := tkVrmlProto
  else
    if KeyComp('FACE') then
      Result := tkVrmlParameter
    else
      if KeyComp('if') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func17 :TtkTokenKind;
begin
  if KeyComp('back') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func18 :TtkTokenKind;
begin
  if KeyComp('head') then
    Result := tkKey
  else
    if KeyComp('big') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func19 :TtkTokenKind;
begin
  if KeyComp('do') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func22 :TtkTokenKind;
begin
  if KeyComp('abs') then
    Result := tkNonReservedKey
  else
    if KeyComp('go') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func23 :TtkTokenKind;
begin
  if KeyComp('in') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func25 :TtkTokenKind;
begin
  if KeyComp('ALL') then
    Result := tkVrmlParameter
  else
    if KeyComp('Area') then
      Result := tkNonReservedKey
    else
      if KeyComp('PI') then
        Result := tkNonReservedKey
      else
        if KeyComp('All') then
          Result := tkNonReservedKey
        else
          if KeyComp('all') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func26 :TtkTokenKind;
begin
  if KeyComp('Arc2D') then
    Result := tkVrmlShape
  else
    if KeyComp('LN10') then
      Result := tkNonReservedKey
    else
      if KeyComp('LN2') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func27 :TtkTokenKind;
begin
  if KeyComp('OFF') then
    Result := tkVrmlParameter
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func28 :TtkTokenKind;
begin
  if KeyComp('X3D') then
    Result := tkVrmlProto
  else
    if KeyComp('IS') then
      Result := tkVrmlProto
    else
      if KeyComp('Fog') then
        Result := tkVrmlNode
      else
        if KeyComp('case') then
          Result := tkKey
        else
          if KeyComp('call') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func29 :TtkTokenKind;
begin
  if KeyComp('ON') then
    Result := tkVrmlParameter
  else
    if KeyComp('on') then
      Result := tkVrmlAttribute
    else
      if KeyComp('ccw') then
        Result := tkVrmlAttribute
      else
        if KeyComp('NaN') then
          Result := tkKey
        else
          if KeyComp('Embed') then
            Result := tkNonReservedKey
          else
            if KeyComp('ceil') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func30 :TtkTokenKind;
begin
  if KeyComp('Date') then
    Result := tkNonReservedKey
  else
    if KeyComp('char') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func31 :TtkTokenKind;
begin
  if KeyComp('LOD') then
    Result := tkVrmlGrouping
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func32 :TtkTokenKind;
begin
  if KeyComp('FILE') then
    Result := tkVrmlParameter
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func33 :TtkTokenKind;
begin
  if KeyComp('BOLD') then
    Result := tkVrmlParameter
  else
    if KeyComp('bold') then
      Result := tkNonReservedKey
    else
      if KeyComp('name') then
        Result := tkNonReservedKey
      else
        if KeyComp('find') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func34 :TtkTokenKind;
begin
  if KeyComp('log') then
    Result := tkNonReservedKey
  else
    if KeyComp('java') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func35 :TtkTokenKind;
begin
  if KeyComp('TO') then
    Result := tkVrmlProto
  else
    if KeyComp('Image') then
      Result := tkNonReservedKey
    else
      if KeyComp('image') then
        Result := tkVrmlAttribute
      else
        if KeyComp('tan') then
          Result := tkNonReservedKey
        else
          if KeyComp('catch') then
            Result := tkKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func36 :TtkTokenKind;
begin
  if KeyComp('field') then
    Result := tkVrmlEvent
  else
    if KeyComp('min') then
      Result := tkNonReservedKey
    else
      if KeyComp('hash') then
        Result := tkNonReservedKey
      else
        if KeyComp('atan2') then
          Result := tkNonReservedKey
        else
          if KeyComp('atan') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func37 :TtkTokenKind;
begin
  if KeyComp('side') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Cone') then
      Result := tkVrmlShape
    else
      if KeyComp('break') then
        Result := tkKey
      else
        if KeyComp('href') then
          Result := tkNonReservedKey
        else
          if KeyComp('cos') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func38 :TtkTokenKind;
begin
  if KeyComp('click') then
    Result := tkNonReservedKey
  else
    if KeyComp('acos') then
      Result := tkNonReservedKey
    else
      if KeyComp('max') then
        Result := tkNonReservedKey
      else
        if KeyComp('callee') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func39 :TtkTokenKind;
begin
  if KeyComp('meta') then
    Result := tkKey
  else
    if KeyComp('LOG10E') then
      Result := tkNonReservedKey
    else
      if KeyComp('LOG2E') then
        Result := tkNonReservedKey
      else
        if KeyComp('checked') then
          Result := tkNonReservedKey
        else
          if KeyComp('clear') then
            Result := tkNonReservedKey
          else
            if KeyComp('for') then
              Result := tkKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func40 :TtkTokenKind;
begin
  if KeyComp('scale') then
    Result := tkVrmlAttribute
  else
    if KeyComp('eval') then
      Result := tkNonReservedKey
    else
      if KeyComp('src') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func41 :TtkTokenKind;
begin
  if KeyComp('key') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Box') then
      Result := tkVrmlShape
    else
      if KeyComp('else') then
        Result := tkKey
      else
        if KeyComp('var') then
          Result := tkKey
        else
          if KeyComp('home') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func42 :TtkTokenKind;
begin
  if KeyComp('self') then
    Result := tkNonReservedKey
  else
    if KeyComp('Math') then
      Result := tkNonReservedKey
    else
      if KeyComp('sin') then
        Result := tkNonReservedKey
      else
        if KeyComp('new') then
          Result := tkKey
        else
          if KeyComp('sub') then
            Result := tkNonReservedKey
          else
            if KeyComp('final') then
              Result := tkKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func43 :TtkTokenKind;
begin
  if KeyComp('LEFT') then
    Result := tkVrmlParameter
  else
    if KeyComp('choice') then
      Result := tkVrmlAttribute
    else
      if KeyComp('endCap') then
        Result := tkVrmlAttribute
      else
        if KeyComp('FALSE') then
          Result := tkVrmlParameter
        else
          if KeyComp('FALSE') then
            Result := tkVrmlProto
          else
            if KeyComp('enabled') then
              Result := tkVrmlAttribute
            else
              if KeyComp('asin') then
                Result := tkNonReservedKey
              else
                if KeyComp('Frame') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('false') then
                    Result := tkKey
                  else
                    if KeyComp('int') then
                      Result := tkKey
                    else
                      if KeyComp('left') then
                        Result := tkNonReservedKey
                      else
                        if KeyComp('align') then
                          Result := tkNonReservedKey
                        else
                          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func44 :TtkTokenKind;
begin
  if KeyComp('info') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Hidden') then
      Result := tkNonReservedKey
    else
      if KeyComp('UTC') then
        Result := tkNonReservedKey
      else
        if KeyComp('package') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func45 :TtkTokenKind;
begin
  if KeyComp('range') then
    Result := tkVrmlAttribute
  else
    if KeyComp('USE') then
      Result := tkVrmlProto
    else
      if KeyComp('CLAMP') then
        Result := tkVrmlParameter
      else
        if KeyComp('exp') then
          Result := tkNonReservedKey
        else
          if KeyComp('match') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func46 :TtkTokenKind;
begin
  if KeyComp('Scene') then
    Result := tkVrmlProto
  else
    if KeyComp('Link') then
      Result := tkNonReservedKey
    else
      if KeyComp('link') then
        Result := tkNonReservedKey
      else
        if KeyComp('body') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func47 :TtkTokenKind;
begin
  if KeyComp('Disk2D') then
    Result := tkVrmlShape
  else
    if KeyComp('time') then
      Result := tkVrmlAttribute
    else
      if KeyComp('Radio') then
        Result := tkNonReservedKey
      else
        if KeyComp('tags') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func48 :TtkTokenKind;
begin
  if KeyComp('NONE') then
    Result := tkVrmlParameter
  else
    if KeyComp('join') then
      Result := tkNonReservedKey
    else
      if KeyComp('embeds') then
        Result := tkNonReservedKey
      else
        if KeyComp('blink') then
          Result := tkNonReservedKey
        else
          if KeyComp('fixed') then
            Result := tkNonReservedKey
          else
            if KeyComp('slice') then
              Result := tkNonReservedKey
            else
              if KeyComp('long') then
                Result := tkKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func49 :TtkTokenKind;
begin
  if KeyComp('xml') then
    Result := tkKey
  else
    if KeyComp('Shape') then
      Result := tkVrmlShape
    else
      if KeyComp('speed') then
        Result := tkVrmlAttribute
      else
        if KeyComp('SHAPE') then
          Result := tkVrmlParameter
        else
          if KeyComp('escape') then
            Result := tkNonReservedKey
          else
            if KeyComp('Global') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func50 :TtkTokenKind;
begin
  if KeyComp('open') then
    Result := tkNonReservedKey
  else
    if KeyComp('void') then
      Result := tkKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func51 :TtkTokenKind;
begin
  if KeyComp('top') then
    Result := tkVrmlAttribute
  else
    if KeyComp('url') then
      Result := tkVrmlAttribute
    else
      if KeyComp('charAt') then
        Result := tkNonReservedKey
      else
        if KeyComp('URL') then
          Result := tkNonReservedKey
        else
          if KeyComp('caller') then
            Result := tkNonReservedKey
          else
            if KeyComp('delete') then
              Result := tkKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func52 :TtkTokenKind;
begin
  if KeyComp('Form') then
    Result := tkNonReservedKey
  else
    if KeyComp('form') then
      Result := tkNonReservedKey
    else
      if KeyComp('hspace') then
        Result := tkNonReservedKey
      else
        if KeyComp('byte') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func53 :TtkTokenKind;
begin
  if KeyComp('Site') then
    Result := tkVrmlNode
  else
    if KeyComp('Shape2D') then
      Result := tkVrmlShape
    else
      if KeyComp('minBack') then
        Result := tkVrmlAttribute
      else
        if KeyComp('blur') then
          Result := tkNonReservedKey
        else
          if KeyComp('enum') then
            Result := tkKey
          else
            if KeyComp('pageX') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func54 :TtkTokenKind;
begin
  if KeyComp('Circle2D') then
    Result := tkVrmlShape
  else
    if KeyComp('ITALIC') then
      Result := tkVrmlParameter
    else
      if KeyComp('pow') then
        Result := tkNonReservedKey
      else
        if KeyComp('close') then
          Result := tkNonReservedKey
        else
          if KeyComp('search') then
            Result := tkNonReservedKey
          else
            if KeyComp('images') then
              Result := tkNonReservedKey
            else
              if KeyComp('class') then
                Result := tkKey
              else
                if KeyComp('float') then
                  Result := tkKey
                else
                  if KeyComp('Float') then
                    Result := tkNonReservedKey
                  else
                    if KeyComp('pageY') then
                      Result := tkNonReservedKey
                    else
                      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func55 :TtkTokenKind;
begin
  if KeyComp('MFVec2f') then
    Result := tkVrmlDefinition
  else
    if KeyComp('coord') then
      Result := tkVrmlAttribute
    else
      if KeyComp('maxBack') then
        Result := tkVrmlAttribute
      else
        if KeyComp('MFVec3f') then
          Result := tkVrmlDefinition
        else
          if KeyComp('reload') then
            Result := tkNonReservedKey
          else
            if KeyComp('Object') then
              Result := tkNonReservedKey
            else
              if KeyComp('watch') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func56 :TtkTokenKind;
begin
  if KeyComp('SIDES') then
    Result := tkVrmlParameter
  else
    if KeyComp('level') then
      Result := tkVrmlAttribute
    else
      if KeyComp('pitch') then
        Result := tkVrmlAttribute
      else
        if KeyComp('this') then
          Result := tkKey
        else
          if KeyComp('alert') then
            Result := tkNonReservedKey
          else
            if KeyComp('sup') then
              Result := tkNonReservedKey
            else
              if KeyComp('domain') then
                Result := tkNonReservedKey
              else
                if KeyComp('index') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('concat') then
                    Result := tkNonReservedKey
                  else
                    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func57 :TtkTokenKind;
begin
  if KeyComp('beginCap') then
    Result := tkVrmlAttribute
  else
    if KeyComp('height') then
      Result := tkVrmlAttribute
    else
      if KeyComp('MFNode') then
        Result := tkVrmlDefinition
      else
        if KeyComp('AUTO') then
          Result := tkVrmlParameter
        else
          if KeyComp('isNaN') then
            Result := tkNonReservedKey
          else
            if KeyComp('small') then
              Result := tkNonReservedKey
            else
              if KeyComp('while') then
                Result := tkKey
              else
                if KeyComp('height') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('goto') then
                    Result := tkKey
                  else
                    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func58 :TtkTokenKind;
begin
  if KeyComp('GeoLOD') then
    Result := tkVrmlAttribute
  else
    if KeyComp('WRAP') then
      Result := tkVrmlParameter
    else
      if KeyComp('loop') then
        Result := tkVrmlAttribute
      else
        if KeyComp('cookie') then
          Result := tkNonReservedKey
        else
          if KeyComp('closed') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func59 :TtkTokenKind;
begin
  if KeyComp('Anchor') then
    Result := tkVrmlGrouping
  else
    if KeyComp('SOLID') then
      Result := tkVrmlParameter
    else
      if KeyComp('size') then
        Result := tkVrmlAttribute
      else
        if KeyComp('NULL') then
          Result := tkVrmlParameter
        else
          if KeyComp('solid') then
            Result := tkVrmlAttribute
          else
            if KeyComp('NULL') then
              Result := tkVrmlProto
            else
              if KeyComp('parse') then
                Result := tkNonReservedKey
              else
                if KeyComp('anchor') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('double') then
                    Result := tkKey
                  else
                    if KeyComp('Null') then
                      Result := tkNonReservedKey
                    else
                      if KeyComp('null') then
                        Result := tkKey
                      else
                        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func60 :TtkTokenKind;
begin
  if KeyComp('SFImage') then
    Result := tkVrmlDefinition
  else
    if KeyComp('jump') then
      Result := tkVrmlAttribute
    else
      if KeyComp('collide') then
        Result := tkVrmlAttribute
      else
        if KeyComp('with') then
          Result := tkKey
        else
          if KeyComp('replace') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func61 :TtkTokenKind;
begin
  if KeyComp('SFVec3f') then
    Result := tkVrmlDefinition
  else
    if KeyComp('SFVec2f') then
      Result := tkVrmlDefinition
    else
      if KeyComp('onLoad') then
        Result := tkEvent
      else
        if KeyComp('value') then
          Result := tkNonReservedKey
        else
          if KeyComp('Layer') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func62 :TtkTokenKind;
begin
  if KeyComp('MFInt32') then
    Result := tkVrmlDefinition
  else
    if KeyComp('RIGHT') then
      Result := tkVrmlParameter
    else
      if KeyComp('action') then
        Result := tkNonReservedKey
      else
        if KeyComp('getDate') then
          Result := tkNonReservedKey
        else
          if KeyComp('getDay') then
            Result := tkNonReservedKey
          else
            if KeyComp('border') then
              Result := tkNonReservedKey
            else
              if KeyComp('host') then
                Result := tkNonReservedKey
              else
                if KeyComp('frames') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('right') then
                    Result := tkNonReservedKey
                  else
                    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func63 :TtkTokenKind;
begin
  if KeyComp('PUBLIC') then
    Result := tkKey
  else
    if KeyComp('color') then
      Result := tkVrmlAttribute
    else
      if KeyComp('Inline') then
        Result := tkVrmlGrouping
      else
        if KeyComp('SFNode') then
          Result := tkVrmlDefinition
        else
          if KeyComp('spine') then
            Result := tkVrmlAttribute
          else
            if KeyComp('Color') then
              Result := tkVrmlNode
            else
              if KeyComp('Array') then
                Result := tkNonReservedKey
              else
                if KeyComp('next') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('try') then
                    Result := tkKey
                  else
                    if KeyComp('public') then
                      Result := tkKey
                    else
                      if KeyComp('Packages') then
                        Result := tkNonReservedKey
                      else
                        if KeyComp('logon') then
                          Result := tkNonReservedKey
                        else
                          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func64 :TtkTokenKind;
begin
  if KeyComp('Boolean') then
    Result := tkNonReservedKey
  else
    if KeyComp('Select') then
      Result := tkNonReservedKey
    else
      if KeyComp('select') then
        Result := tkNonReservedKey
      else
        if KeyComp('taint') then
          Result := tkNonReservedKey
        else
          if KeyComp('focus') then
            Result := tkNonReservedKey
          else
            if KeyComp('boolean') then
              Result := tkKey
            else
              if KeyComp('width') then
                Result := tkNonReservedKey
              else
                if KeyComp('TRUE') then
                  Result := tkVrmlParameter
                else
                  if KeyComp('true') then
                    Result := tkKey
                  else
                    if KeyComp('screen') then
                      Result := tkNonReservedKey
                    else
                      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func65 :TtkTokenKind;
begin
  if KeyComp('CENTER') then
    Result := tkVrmlParameter
  else
    if KeyComp('REPEAT') then
      Result := tkVrmlParameter
    else
      if KeyComp('VRML') then
        Result := tkVrmlProto
      else
        if KeyComp('center') then
          Result := tkVrmlAttribute
        else
          if KeyComp('filename') then
            Result := tkNonReservedKey
          else
            if KeyComp('links') then
              Result := tkNonReservedKey
            else
              if KeyComp('method') then
                Result := tkNonReservedKey
              else
                if KeyComp('random') then
                  Result := tkNonReservedKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func66 :TtkTokenKind;
begin
  if KeyComp('X3DNode') then
    Result := tkVrmlProto
  else
    if KeyComp('FAMILY') then
      Result := tkVrmlParameter
    else
      if KeyComp('length') then
        Result := tkVrmlAttribute
      else
        if KeyComp('family') then
          Result := tkVrmlAttribute
        else
          if KeyComp('TYPE') then
            Result := tkVrmlParameter
          else
            if KeyComp('type') then
              Result := tkVrmlAttribute
            else
              if KeyComp('MFTime') then
                Result := tkVrmlDefinition
              else
                if KeyComp('vspace') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('title') then
                    Result := tkNonReservedKey
                  else
                    if KeyComp('type') then
                      Result := tkNonReservedKey
                    else
                      if KeyComp('appName') then
                        Result := tkNonReservedKey
                      else
                        if KeyComp('floor') then
                          Result := tkNonReservedKey
                        else
                          if KeyComp('event') then
                            Result := tkNonReservedKey
                          else
                            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func67 :TtkTokenKind;
begin
  if KeyComp('onClick') then
    Result := tkEvent
  else
    if KeyComp('onChange') then
      Result := tkEvent
    else
      if KeyComp('reset') then
        Result := tkNonReservedKey
      else
        if KeyComp('Reset') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func68 :TtkTokenKind;
begin
  if KeyComp('Joint') then
    Result := tkVrmlNode
  else
    if KeyComp('backUrl') then
      Result := tkVrmlAttribute
    else
      if KeyComp('SFInt32') then
        Result := tkVrmlDefinition
      else
        if KeyComp('language') then
          Result := tkVrmlAttribute
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func69 :TtkTokenKind;
begin
  if KeyComp('SFBool') then
    Result := tkVrmlDefinition
  else
    if KeyComp('fieldName') then
      Result := tkVrmlAttribute
    else
      if KeyComp('Text') then
        Result := tkVrmlShape
      else
        if KeyComp('spacing') then
          Result := tkVrmlAttribute
        else
          if KeyComp('DEFAULT') then
            Result := tkVrmlParameter
          else
            if KeyComp('port') then
              Result := tkNonReservedKey
            else
              if KeyComp('Text') then
                Result := tkNonReservedKey
              else
                if KeyComp('text') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('default') then
                    Result := tkKey
                  else
                    if KeyComp('debugger') then
                      Result := tkKey
                    else
                      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func70 :TtkTokenKind;
begin
  if KeyComp('Applet') then
    Result := tkNonReservedKey
  else
    if KeyComp('stop') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func71 :TtkTokenKind;
begin
  if KeyComp('Sphere') then
    Result := tkVrmlShape
  else
    if KeyComp('offset') then
      Result := tkVrmlAttribute
    else
      if KeyComp('target') then
        Result := tkNonReservedKey
      else
        if KeyComp('Checkbox') then
          Result := tkNonReservedKey
        else
          if KeyComp('encoding') then
            Result := tkNonReservedKey
          else
            if KeyComp('forms') then
              Result := tkNonReservedKey
            else
              if KeyComp('const') then
                Result := tkKey
              else
                if KeyComp('native') then
                  Result := tkKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func72 :TtkTokenKind;
begin
  if KeyComp('SFTime') then
    Result := tkVrmlDefinition
  else
    if KeyComp('radius') then
      Result := tkVrmlAttribute
    else
      if KeyComp('ENUMS') then
        Result := tkVrmlParameter
      else
        if KeyComp('round') then
          Result := tkNonReservedKey
        else
          if KeyComp('sort') then
            Result := tkNonReservedKey
          else
            if KeyComp('bgColor') then
              Result := tkNonReservedKey
            else
              if KeyComp('static') then
                Result := tkKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func73 :TtkTokenKind;
begin
  if KeyComp('children') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Sound') then
      Result := tkVrmlNode
    else
      if KeyComp('MFFloat') then
        Result := tkVrmlDefinition
      else
        if KeyComp('FORMAT') then
          Result := tkVrmlParameter
        else
          if KeyComp('normal') then
            Result := tkVrmlAttribute
          else
            if KeyComp('Normal') then
              Result := tkVrmlNode
            else
              if KeyComp('italics') then
                Result := tkNonReservedKey
              else
                if KeyComp('Number') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('opener') then
                    Result := tkNonReservedKey
                  else
                    if KeyComp('selected') then
                      Result := tkNonReservedKey
                    else
                      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func74 :TtkTokenKind;
begin
  if KeyComp('PARTS') then
    Result := tkVrmlParameter
  else
    if KeyComp('headlight') then
      Result := tkVrmlAttribute
    else
      if KeyComp('point') then
        Result := tkVrmlAttribute
      else
        if KeyComp('sqrt') then
          Result := tkNonReservedKey
        else
          if KeyComp('SQRT2') then
            Result := tkNonReservedKey
          else
            if KeyComp('parent') then
              Result := tkNonReservedKey
            else
              if KeyComp('setDate') then
                Result := tkNonReservedKey
              else
                if KeyComp('menubar') then
                  Result := tkNonReservedKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func75 :TtkTokenKind;
begin
  if KeyComp('minAngle') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Billboard') then
      Result := tkVrmlGrouping
    else
      if KeyComp('write') then
        Result := tkNonReservedKey
      else
        if KeyComp('RegExp') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func76 :TtkTokenKind;
begin
  if KeyComp('bindTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('fgColor') then
      Result := tkNonReservedKey
    else
      if KeyComp('split') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func77 :TtkTokenKind;
begin
  if KeyComp('Group') then
    Result := tkVrmlGrouping
  else
    if KeyComp('maxAngle') then
      Result := tkVrmlAttribute
    else
      if KeyComp('javaEnabled') then
        Result := tkNonReservedKey
      else
        if KeyComp('indexOf') then
          Result := tkNonReservedKey
        else
          if KeyComp('print') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func78 :TtkTokenKind;
begin
  if KeyComp('BINDINGS') then
    Result := tkVrmlParameter
  else
    if KeyComp('CULLING') then
      Result := tkVrmlParameter
    else
      if KeyComp('anchors') then
        Result := tkNonReservedKey
      else
        if KeyComp('confirm') then
          Result := tkNonReservedKey
        else
          if KeyComp('pathname') then
            Result := tkNonReservedKey
          else
            if KeyComp('start') then
              Result := tkKey
            else
              if KeyComp('charCodeAt') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func79 :TtkTokenKind;
begin
  if KeyComp('Material') then
    Result := tkVrmlAppearance
  else
    if KeyComp('material') then
      Result := tkVrmlAttribute
    else
      if KeyComp('SFFloat') then
        Result := tkVrmlDefinition
      else
        if KeyComp('ROUTE') then
          Result := tkVrmlProto
        else
          if KeyComp('Plugin') then
            Result := tkNonReservedKey
          else
            if KeyComp('getTime') then
              Result := tkNonReservedKey
            else
              if KeyComp('refresh') then
                Result := tkNonReservedKey
              else
                if KeyComp('scroll') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('finally') then
                    Result := tkKey
                  else
                    if KeyComp('super') then
                      Result := tkKey
                    else
                      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func80 :TtkTokenKind;
begin
  if KeyComp('appearance') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Appearance') then
      Result := tkVrmlAppearance
    else
      if KeyComp('short') then
        Result := tkKey
      else
        if KeyComp('layers') then
          Result := tkNonReservedKey
        else
          if KeyComp('input') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func81 :TtkTokenKind;
begin
  if KeyComp('ArcClose2D') then
    Result := tkVrmlShape
  else
    if KeyComp('source') then
      Result := tkVrmlAttribute
    else
      if KeyComp('style') then
        Result := tkVrmlAttribute
      else
        if KeyComp('STYLE') then
          Result := tkVrmlParameter
        else
          if KeyComp('getYear') then
            Result := tkNonReservedKey
          else
            if KeyComp('interface') then
              Result := tkKey
            else
              if KeyComp('style') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func82 :TtkTokenKind;
begin
  if KeyComp('diskAngle') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Switch') then
      Result := tkVrmlGrouping
    else
      if KeyComp('MFColor') then
        Result := tkVrmlDefinition
      else
        if KeyComp('addChildren') then
          Result := tkVrmlAttribute
        else
          if KeyComp('onBlur') then
            Result := tkEvent
          else
            if KeyComp('strike') then
              Result := tkNonReservedKey
            else
              if KeyComp('valueOf') then
                Result := tkNonReservedKey
              else
                if KeyComp('moveBy') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('switch') then
                    Result := tkKey
                  else
                    if KeyComp('zIndex') then
                      Result := tkNonReservedKey
                    else
                      if KeyComp('Undefined') then
                        Result := tkNonReservedKey
                      else
                        if KeyComp('undefined') then
                          Result := tkNonReservedKey
                        else
                          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func83 :TtkTokenKind;
begin
  if KeyComp('Segment') then
    Result := tkVrmlShape
  else
    if KeyComp('CONVEX') then
      Result := tkVrmlParameter
    else
      if KeyComp('vector') then
        Result := tkVrmlAttribute
      else
        if KeyComp('convex') then
          Result := tkVrmlAttribute
        else
          if KeyComp('netscape') then
            Result := tkNonReservedKey
          else
            if KeyComp('toolbar') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func84 :TtkTokenKind;
begin
  if KeyComp('repeatS') then
    Result := tkVrmlAttribute
  else
    if KeyComp('PROTO') then
      Result := tkVrmlProto
    else
      if KeyComp('isBound') then
        Result := tkVrmlAttribute
      else
        if KeyComp('Submit') then
          Result := tkNonReservedKey
        else
          if KeyComp('submit') then
            Result := tkNonReservedKey
          else
            if KeyComp('unescape') then
              Result := tkNonReservedKey
            else
              if KeyComp('throw') then
                Result := tkKey
              else
                if KeyComp('abstract') then
                  Result := tkKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func85 :TtkTokenKind;
begin
  if KeyComp('Humanoid') then
    Result := tkVrmlShape
  else
    if KeyComp('PER_FACE') then
      Result := tkVrmlParameter
    else
      if KeyComp('OVERALL') then
        Result := tkVrmlParameter
      else
        if KeyComp('beamWidth') then
          Result := tkVrmlAttribute
        else
          if KeyComp('bottom') then
            Result := tkVrmlAttribute
          else
            if KeyComp('repeatT') then
              Result := tkVrmlAttribute
            else
              if KeyComp('BOTTOM') then
                Result := tkVrmlParameter
              else
                if KeyComp('Script') then
                  Result := tkVrmlNode
                else
                  if KeyComp('onAbort') then
                    Result := tkEvent
                  else
                    if KeyComp('forward') then
                      Result := tkNonReservedKey
                    else
                      if KeyComp('onDblClick') then
                        Result := tkEvent
                      else
                        if KeyComp('bottom') then
                          Result := tkNonReservedKey
                        else
                          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func86 :TtkTokenKind;
begin
  if KeyComp('display') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func87 :TtkTokenKind;
begin
  if KeyComp('Displacer') then
    Result := tkVrmlParameter
  else
    if KeyComp('string') then
      Result := tkVrmlAttribute
    else
      if KeyComp('String') then
        Result := tkNonReservedKey
      else
        if KeyComp('typeof') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func88 :TtkTokenKind;
begin
  if KeyComp('DOCTYPE') then
    Result := tkKey
  else
    if KeyComp('SFColor') then
      Result := tkVrmlDefinition
    else
      if KeyComp('isOver') then
        Result := tkVrmlAttribute
      else
        if KeyComp('isActive') then
          Result := tkVrmlAttribute
        else
          if KeyComp('DEFAULTS') then
            Result := tkVrmlParameter
          else
            if KeyComp('Window') then
              Result := tkNonReservedKey
            else
              if KeyComp('window') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func89 :TtkTokenKind;
begin
  if KeyComp('Rectangle2D') then
    Result := tkVrmlShape
  else
    if KeyComp('eventIn') then
      Result := tkVrmlEvent
    else
      if KeyComp('location') then
        Result := tkVrmlAttribute
      else
        if KeyComp('Location') then
          Result := tkNonReservedKey
        else
          if KeyComp('location') then
            Result := tkNonReservedKey
          else
            if KeyComp('complete') then
              Result := tkNonReservedKey
            else
              if KeyComp('applets') then
                Result := tkNonReservedKey
              else
                if KeyComp('Option') then
                  Result := tkNonReservedKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func90 :TtkTokenKind;
begin
  if KeyComp('creaseAngle') then
    Result := tkVrmlAttribute
  else
    if KeyComp('AudioClip') then
      Result := tkVrmlTime_dependent
    else
      if KeyComp('Cylinder') then
        Result := tkVrmlShape
      else
        if KeyComp('lowsrc') then
          Result := tkNonReservedKey
        else
          if KeyComp('moveTo') then
            Result := tkNonReservedKey
          else
            if KeyComp('unwatch') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func91 :TtkTokenKind;
begin
  if KeyComp('ColorRGBA') then
    Result := tkVrmlAttribute
  else
    if KeyComp('content') then
      Result := tkNonReservedKey
    else
      if KeyComp('setTime') then
        Result := tkNonReservedKey
      else
        if KeyComp('import') then
          Result := tkKey
        else
          if KeyComp('extends') then
            Result := tkKey
          else
            if KeyComp('private') then
              Result := tkKey
            else
              if KeyComp('isFinite') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func92 :TtkTokenKind;
begin
  if KeyComp('GeoMetadata') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Button') then
      Result := tkNonReservedKey
    else
      if KeyComp('reverse') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func93 :TtkTokenKind;
begin
  if KeyComp('xSpacing') then
    Result := tkVrmlAttribute
  else
    if KeyComp('appCodeName') then
      Result := tkNonReservedKey
    else
      if KeyComp('setYear') then
        Result := tkNonReservedKey
      else
        if KeyComp('referrer') then
          Result := tkNonReservedKey
        else
          if KeyComp('elements') then
            Result := tkNonReservedKey
          else
            if KeyComp('onFocus') then
              Result := tkEvent
            else
              if KeyComp('onSelect') then
                Result := tkEvent
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func94 :TtkTokenKind;
begin
  if KeyComp('whichChoice') then
    Result := tkVrmlAttribute
  else
    if KeyComp('fogType') then
      Result := tkVrmlAttribute
    else
      if KeyComp('leftUrl') then
        Result := tkVrmlAttribute
      else
        if KeyComp('skyAngle') then
          Result := tkVrmlAttribute
        else
          if KeyComp('Textarea') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func95 :TtkTokenKind;
begin
  if KeyComp('cycleTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('zSpacing') then
      Result := tkVrmlAttribute
    else
      if KeyComp('hostname') then
        Result := tkNonReservedKey
      else
        if KeyComp('document') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func96 :TtkTokenKind;
begin
  if KeyComp('Background') then
    Result := tkVrmlNode
  else
    if KeyComp('onUnload') then
      Result := tkEvent
    else
      if KeyComp('return') then
        Result := tkKey
      else
        if KeyComp('onReset') then
          Result := tkEvent
        else
          if KeyComp('background') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func97 :TtkTokenKind;
begin
  if KeyComp('direction') then
    Result := tkVrmlAttribute
  else
    if KeyComp('parameter') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func98 :TtkTokenKind;
begin
  if KeyComp('proxy') then
    Result := tkVrmlAttribute
  else
    if KeyComp('prompt') then
      Result := tkNonReservedKey
    else
      if KeyComp('plugins') then
        Result := tkNonReservedKey
      else
        if KeyComp('export') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func99 :TtkTokenKind;
begin
  if KeyComp('GeoOrigin') then
    Result := tkVrmlAttribute
  else
    if KeyComp('eventName') then
      Result := tkVrmlAttribute
    else
      if KeyComp('current') then
        Result := tkNonReservedKey
      else
        if KeyComp('untaint') then
          Result := tkNonReservedKey
        else
          if KeyComp('substr') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func100 :TtkTokenKind;
begin
  if KeyComp('CLOCKWISE') then
    Result := tkVrmlParameter
  else
    if KeyComp('status') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func101 :TtkTokenKind;
begin
  if KeyComp('FileUpload') then
    Result := tkNonReservedKey
  else
    if KeyComp('writeln') then
      Result := tkNonReservedKey
    else
      if KeyComp('continue') then
        Result := tkKey
      else
        if KeyComp('platform') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func102 :TtkTokenKind;
begin
  if KeyComp('X3DChildNode') then
    Result := tkVrmlProto
  else
    if KeyComp('version') then
      Result := tkNonReservedKey
    else
      if KeyComp('keyValue') then
        Result := tkVrmlAttribute
      else
        if KeyComp('fieldType') then
          Result := tkVrmlAttribute
        else
          if KeyComp('bboxSize') then
            Result := tkVrmlAttribute
          else
            if KeyComp('topUrl') then
              Result := tkVrmlAttribute
            else
              if KeyComp('getMonth') then
                Result := tkNonReservedKey
              else
                if KeyComp('Function') then
                  Result := tkNonReservedKey
                else
                  if KeyComp('function') then
                    Result := tkKey
                  else
                    if KeyComp('parseInt') then
                      Result := tkNonReservedKey
                    else
                      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func103 :TtkTokenKind;
begin
  if KeyComp('SignalPdu') then
    Result := tkVrmlGrouping
  else
    if KeyComp('onError') then
      Result := tkEvent
    else
      if KeyComp('throws') then
        Result := tkKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func104 :TtkTokenKind;
begin
  if KeyComp('set_bind') then
    Result := tkVrmlAttribute
  else
    if KeyComp('texCoord') then
      Result := tkVrmlAttribute
    else
      if KeyComp('Coordinate') then
        Result := tkVrmlNode
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func105 :TtkTokenKind;
begin
  if KeyComp('exitTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('SQRT1_2') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func106 :TtkTokenKind;
begin
  if KeyComp('MFString') then
    Result := tkVrmlDefinition
  else
    if KeyComp('MimeType') then
      Result := tkNonReservedKey
    else
      if KeyComp('instanceof') then
        Result := tkKey
      else
        if KeyComp('protected') then
          Result := tkKey
        else
          if KeyComp('Infinity') then
            Result := tkNonReservedKey
          else
            if KeyComp('scrollBy') then
              Result := tkNonReservedKey
            else
              if KeyComp('getUTCDate') then
                Result := tkNonReservedKey
              else
                if KeyComp('getUTCDay') then
                  Result := tkNonReservedKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func107 :TtkTokenKind;
begin
  if KeyComp('collideTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('taintEnabled') then
      Result := tkNonReservedKey
    else
      if KeyComp('Navigator') then
        Result := tkNonReservedKey
      else
        if KeyComp('navigator') then
          Result := tkNonReservedKey
        else
          if KeyComp('onKeyUp') then
            Result := tkEvent
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func108 :TtkTokenKind;
begin
  if KeyComp('Collision') then
    Result := tkVrmlGrouping
  else
    if KeyComp('bboxCenter') then
      Result := tkVrmlAttribute
    else
      if KeyComp('geometry') then
        Result := tkVrmlAttribute
      else
        if KeyComp('defaultChecked') then
          Result := tkNonReservedKey
        else
          if KeyComp('options') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func109 :TtkTokenKind;
begin
  if KeyComp('enterTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('minFront') then
      Result := tkVrmlAttribute
    else
      if KeyComp('suffixes') then
        Result := tkNonReservedKey
      else
        if KeyComp('linkColor') then
          Result := tkNonReservedKey
        else
          if KeyComp('resizeBy') then
            Result := tkNonReservedKey
          else
            if KeyComp('fromCharCode') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func110 :TtkTokenKind;
begin
  if KeyComp('Contour2D') then
    Result := tkVrmlShape
  else
    if KeyComp('cutOffAngle') then
      Result := tkVrmlAttribute
    else
      if KeyComp('justify') then
        Result := tkVrmlAttribute
      else
        if KeyComp('userAgent') then
          Result := tkNonReservedKey
        else
          if KeyComp('alinkColor') then
            Result := tkNonReservedKey
          else
            if KeyComp('locationbar') then
              Result := tkNonReservedKey
            else
              if KeyComp('handleEvent') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func111 :TtkTokenKind;
begin
  if KeyComp('maxFront') then
    Result := tkVrmlAttribute
  else
    if KeyComp('coordIndex') then
      Result := tkVrmlAttribute
    else
      if KeyComp('getSeconds') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func112 :TtkTokenKind;
begin
  if KeyComp('Polyline2D') then
    Result := tkVrmlShape
  else
    if KeyComp('rotation') then
      Result := tkVrmlAttribute
    else
      if KeyComp('SFString') then
        Result := tkVrmlDefinition
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func113 :TtkTokenKind;
begin
  if KeyComp('rightUrl') then
    Result := tkVrmlAttribute
  else
    if KeyComp('texture') then
      Result := tkVrmlAttribute
    else
      if KeyComp('onSubmit') then
        Result := tkEvent
      else
        if KeyComp('parseFloat') then
          Result := tkNonReservedKey
        else
          if KeyComp('getHours') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func114 :TtkTokenKind;
begin
  if KeyComp('touchTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('fontsize') then
      Result := tkNonReservedKey
    else
      if KeyComp('History') then
        Result := tkNonReservedKey
      else
        if KeyComp('history') then
          Result := tkNonReservedKey
        else
          if KeyComp('setMonth') then
            Result := tkNonReservedKey
          else
            if KeyComp('protocol') then
              Result := tkNonReservedKey
            else
              if KeyComp('scrollTo') then
                Result := tkNonReservedKey
              else
                Result := tkIdentifier;
end;

function TSynVrml97Syn.Func115 :TtkTokenKind;
begin
  if KeyComp('X3DBindableNode') then
    Result := tkVrmlProto
  else
    if KeyComp('X3DShapeNode') then
      Result := tkVrmlProto
    else
      if KeyComp('set_scale') then
        Result := tkVrmlAttribute
      else
        if KeyComp('Password') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func116 :TtkTokenKind;
begin
  if KeyComp('GeoLocation') then
    Result := tkVrmlAttribute
  else
    if KeyComp('fieldOfView') then
      Result := tkVrmlAttribute
    else
      if KeyComp('shininess') then
        Result := tkVrmlAttribute
      else
        if KeyComp('WorldInfo') then
          Result := tkVrmlWorldInfo
        else
          if KeyComp('toSource') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func117 :TtkTokenKind;
begin
  if KeyComp('stopTime') then
    Result := tkVrmlAttribute
  else
    if KeyComp('position') then
      Result := tkVrmlAttribute
    else
      if KeyComp('lastModified') then
        Result := tkNonReservedKey
      else
        if KeyComp('resizeTo') then
          Result := tkNonReservedKey
        else
          if KeyComp('innerHeight') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func118 :TtkTokenKind;
begin
  if KeyComp('spatialize') then
    Result := tkVrmlAttribute
  else
    if KeyComp('groundAngle') then
      Result := tkVrmlAttribute
    else
      if KeyComp('skyColor') then
        Result := tkVrmlAttribute
      else
        if KeyComp('PointSet') then
          Result := tkVrmlShape
        else
          if KeyComp('fontcolor') then
            Result := tkNonReservedKey
          else
            if KeyComp('Arguments') then
              Result := tkNonReservedKey
            else
              if KeyComp('arguments') then
                Result := tkNonReservedKey
              else
                if KeyComp('setUTCDate') then
                  Result := tkNonReservedKey
                else
                  Result := tkIdentifier;
end;

function TSynVrml97Syn.Func119 :TtkTokenKind;
begin
  if KeyComp('colorIndex') then
    Result := tkVrmlAttribute
  else
    if KeyComp('scrollbars') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func120 :TtkTokenKind;
begin
  if KeyComp('transient') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func121 :TtkTokenKind;
begin
  if KeyComp('personalbar') then
    Result := tkNonReservedKey
  else
    if KeyComp('statusbar') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func122 :TtkTokenKind;
begin
  if KeyComp('LoadSensor') then
    Result := tkVrmlSensor
  else
    if KeyComp('X3DLightNode') then
      Result := tkVrmlProto
    else
      if KeyComp('eventOut') then
        Result := tkVrmlEvent
      else
        if KeyComp('avatarSize') then
          Result := tkVrmlAttribute
        else
          if KeyComp('toString') then
            Result := tkNonReservedKey
          else
            if KeyComp('enabledPlugin') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func123 :TtkTokenKind;
begin
  if KeyComp('setSeconds') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func124 :TtkTokenKind;
begin
  if KeyComp('Transform') then
    Result := tkVrmlGrouping
  else
    if KeyComp('IndexedFaceSet') then
      Result := tkVrmlShape_Hint
    else
      if KeyComp('exposedField') then
        Result := tkVrmlEvent
      else
        if KeyComp('frontUrl') then
          Result := tkVrmlAttribute
        else
          if KeyComp('innerWidth') then
            Result := tkNonReservedKey
          else
            if KeyComp('pageXOffset') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func125 :TtkTokenKind;
begin
  if KeyComp('PER_PART') then
    Result := tkVrmlParameter
  else
    if KeyComp('startTime') then
      Result := tkVrmlAttribute
    else
      if KeyComp('previous') then
        Result := tkNonReservedKey
      else
        if KeyComp('setHours') then
          Result := tkNonReservedKey
        else
          if KeyComp('mimeTypes') then
            Result := tkNonReservedKey
          else
            if KeyComp('pageYOffset') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func126 :TtkTokenKind;
begin
  if KeyComp('ReceiverPdu') then
    Result := tkVrmlGrouping
  else
    if KeyComp('SpotLight') then
      Result := tkVrmlLight
    else
      if KeyComp('xDimension') then
        Result := tkVrmlAttribute
      else
        if KeyComp('maxExtent') then
          Result := tkVrmlAttribute
        else
          if KeyComp('implements') then
            Result := tkKey
          else
            if KeyComp('onKeyDown') then
              Result := tkEvent
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func128 :TtkTokenKind;
begin
  if KeyComp('Transform2D') then
    Result := tkVrmlGrouping
  else
    if KeyComp('zDimension') then
      Result := tkVrmlAttribute
    else
      if KeyComp('autoOffset') then
        Result := tkVrmlAttribute
      else
        if KeyComp('MIN_VALUE') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func129 :TtkTokenKind;
begin
  if KeyComp('X3DColorNode') then
    Result := tkVrmlProto
  else
    if KeyComp('normalIndex') then
      Result := tkVrmlAttribute
    else
      if KeyComp('lastIndexOf') then
        Result := tkNonReservedKey
      else
        if KeyComp('substring') then
          Result := tkNonReservedKey
        else
          if KeyComp('selectedIndex') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func130 :TtkTokenKind;
begin
  if KeyComp('TriangleSet') then
    Result := tkVrmlShape
  else
    if KeyComp('BooleanToggle') then
      Result := tkVrmlSensor
    else
      if KeyComp('priority') then
        Result := tkVrmlAttribute
      else
        if KeyComp('PointLight') then
          Result := tkVrmlLight
        else
          if KeyComp('defaultValue') then
            Result := tkNonReservedKey
          else
            if KeyComp('MAX_VALUE') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func131 :TtkTokenKind;
begin
  if KeyComp('TimeTrigger') then
    Result := tkVrmlTime_Dependent
  else
    if KeyComp('KeySensor') then
      Result := tkVrmlSensor
    else
      if KeyComp('GeoCoordinate') then
        Result := tkVrmlAttribute
      else
        if KeyComp('MFRotation') then
          Result := tkVrmlDefinition
        else
          if KeyComp('vlinkColor') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func132 :TtkTokenKind;
begin
  if KeyComp('set_height') then
    Result := tkVrmlAttribute
  else
    if KeyComp('description') then
      Result := tkVrmlAttribute
    else
      if KeyComp('eventType') then
        Result := tkVrmlAttribute
      else
        if KeyComp('description') then
          Result := tkNonReservedKey
        else
          if KeyComp('getFullYear') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func133 :TtkTokenKind;
begin
  if KeyComp('diffuseColor') then
    Result := tkVrmlAttribute
  else
    if KeyComp('Viewpoint') then
      Result := tkVrmlViewpoint
    else
      if KeyComp('getMinutes') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func134 :TtkTokenKind;
begin
  if KeyComp('TriangleSet2D') then
    Result := tkVrmlShape
  else
    if KeyComp('BooleanFilter') then
      Result := tkVrmlSensor
    else
      if KeyComp('X3DUrlObject') then
        Result := tkVrmlProto
      else
        if KeyComp('value_changed') then
          Result := tkVrmlAttribute
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func135 :TtkTokenKind;
begin
  if KeyComp('intensity') then
    Result := tkVrmlAttribute
  else
    if KeyComp('appVersion') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func136 :TtkTokenKind;
begin
  if KeyComp('fontStyle') then
    Result := tkVrmlAttribute
  else
    if KeyComp('bottomUrl') then
      Result := tkVrmlAttribute
    else
      if KeyComp('FontStyle') then
        Result := tkVrmlNode
      else
        if KeyComp('toLowerCase') then
          Result := tkNonReservedKey
        else
          if KeyComp('outerHeight') then
            Result := tkNonReservedKey
          else
            if KeyComp('visibility') then
              Result := tkNonReservedKey
            else
              Result := tkIdentifier;
end;

function TSynVrml97Syn.Func137 :TtkTokenKind;
begin
  if KeyComp('SFRotation') then
    Result := tkVrmlDefinition
  else
    if KeyComp('TimeSensor') then
      Result := tkVrmlSensor
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func138 :TtkTokenKind;
begin
  if KeyComp('horizontal') then
    Result := tkVrmlAttribute
  else
    if KeyComp('PlaneSensor') then
      Result := tkVrmlSensor
    else
      if KeyComp('set_spine') then
        Result := tkVrmlAttribute
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func139 :TtkTokenKind;
begin
  if KeyComp('X3DSoundNode') then
    Result := tkVrmlProto
  else
    if KeyComp('X3DNormalNode') then
      Result := tkVrmlProto
    else
      if KeyComp('toUpperCase') then
        Result := tkNonReservedKey
      else
        if KeyComp('onMouseUp') then
          Result := tkEvent
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func140 :TtkTokenKind;
begin
  if KeyComp('orientation') then
    Result := tkVrmlAttribute
  else
    if KeyComp('attenuation') then
      Result := tkVrmlAttribute
    else
      if KeyComp('leftToRight') then
        Result := tkVrmlAttribute
      else
        if KeyComp('clearInterval') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func141 :TtkTokenKind;
begin
  if KeyComp('ElevationGrid') then
    Result := tkVrmlShape
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func142 :TtkTokenKind;
begin
  if KeyComp('groundColor') then
    Result := tkVrmlAttribute
  else
    if KeyComp('defaultSelected') then
      Result := tkNonReservedKey
    else
      if KeyComp('clearTimeout') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func143 :TtkTokenKind;
begin
  if KeyComp('NurbsCurve') then
    Result := tkVrmlShape
  else
    if KeyComp('translation') then
      Result := tkVrmlAttribute
    else
      if KeyComp('outerWidth') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func144 :TtkTokenKind;
begin
  if KeyComp('setFullYear') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func145 :TtkTokenKind;
begin
  if KeyComp('X3DMaterialNode') then
    Result := tkVrmlProto
  else
    if KeyComp('Extrusion') then
      Result := tkVrmlShape_Hint
    else
      if KeyComp('setMinutes') then
        Result := tkNonReservedKey
      else
        if KeyComp('setInterval') then
          Result := tkNonReservedKey
        else
          if KeyComp('routeEvent') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func146 :TtkTokenKind;
begin
  if KeyComp('Polypoint2D') then
    Result := tkVrmlShape
  else
    if KeyComp('X3DAppearanceNode') then
      Result := tkVrmlProto
    else
      if KeyComp('X3DAppearanceNode') then
        Result := tkVrmlProto
      else
        if KeyComp('getUTCMonth') then
          Result := tkNonReservedKey
        else
          if KeyComp('getElementById') then
            Result := tkNonReservedKey
          else
            Result := tkIdentifier;
end;

function TSynVrml97Syn.Func147 :TtkTokenKind;
begin
  if KeyComp('NurbsSurface') then
    Result := tkVrmlShape
  else
    if KeyComp('NurbsCurve2D') then
      Result := tkVrmlShape
    else
      if KeyComp('setTimeout') then
        Result := tkNonReservedKey
      else
        if KeyComp('onKeyPress') then
          Result := tkEvent
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func148 :TtkTokenKind;
begin
  if KeyComp('BooleanTrigger') then
    Result := tkVrmlSensor
  else
    if KeyComp('X3DBoundedObject') then
      Result := tkVrmlProto
    else
      if KeyComp('ImageTexture') then
        Result := tkVrmlAppearance
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func149 :TtkTokenKind;
begin
  if KeyComp('StaticGroup') then
    Result := tkVrmlGrouping
  else
    if KeyComp('X3DTriggerNode') then
      Result := tkVrmlProto
    else
      if KeyComp('IndexedLineSet') then
        Result := tkVrmlShape_Hint
      else
        if KeyComp('cycleInterval') then
          Result := tkVrmlAttribute
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func150 :TtkTokenKind;
begin
  if KeyComp('prototype') then
    Result := tkKey
  else
    if KeyComp('releaseEvents') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func151 :TtkTokenKind;
begin
  if KeyComp('TriangleFanSet') then
    Result := tkVrmlShape
  else
    if KeyComp('NurbsGroup') then
      Result := tkVrmlGrouping
    else
      if KeyComp('X3DScriptNode') then
        Result := tkVrmlProto
      else
        if KeyComp('removeChildren') then
          Result := tkVrmlAttribute
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func153 :TtkTokenKind;
begin
  if KeyComp('minPosition') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func154 :TtkTokenKind;
begin
  if KeyComp('transparency') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func155 :TtkTokenKind;
begin
  if KeyComp('TrimmedSurface') then
    Result := tkVrmlShape
  else
    if KeyComp('maxPosition') then
      Result := tkVrmlAttribute
    else
      if KeyComp('getUTCSeconds') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func156 :TtkTokenKind;
begin
  if KeyComp('X3DSensorNode') then
    Result := tkVrmlProto
  else
    if KeyComp('JUSTIFICATION') then
      Result := tkVrmlParameter
    else
      if KeyComp('NavigationInfo') then
        Result := tkVrmlNode
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func157 :TtkTokenKind;
begin
  if KeyComp('bottomRadius') then
    Result := tkVrmlAttribute
  else
    if KeyComp('TouchSensor') then
      Result := tkVrmlSensor
    else
      if KeyComp('onMouseMove') then
        Result := tkEvent
      else
        if KeyComp('getUTCHours') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func158 :TtkTokenKind;
begin
  if KeyComp('specularColor') then
    Result := tkVrmlAttribute
  else
    if KeyComp('onMouseOut') then
      Result := tkEvent
    else
      if KeyComp('onMouseDown') then
        Result := tkEvent
      else
        if KeyComp('setUTCMonth') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func159 :TtkTokenKind;
begin
  if KeyComp('crossSection') then
    Result := tkVrmlAttribute
  else
    if KeyComp('fraction_changed') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func160 :TtkTokenKind;
begin
  if KeyComp('GeoViewpoint') then
    Result := tkVrmlViewpoint
  else
    if KeyComp('mustEvaluate') then
      Result := tkVrmlAttribute
    else
      if KeyComp('texCoordIndex') then
        Result := tkVrmlAttribute
      else
        if KeyComp('synchronized') then
          Result := tkKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func161 :TtkTokenKind;
begin
  if KeyComp('SphereSensor') then
    Result := tkVrmlSensor
  else
    if KeyComp('set_fraction') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func162 :TtkTokenKind;
begin
  if KeyComp('IntegerTrigger') then
    Result := tkVrmlSensor
  else
    if KeyComp('X3DBackgroundNode') then
      Result := tkVrmlProto
    else
      if KeyComp('toGMTString') then
        Result := tkNonReservedKey
      else
        if KeyComp('onMouseOver') then
          Result := tkEvent
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func164 :TtkTokenKind;
begin
  if KeyComp('PER_VERTEX') then
    Result := tkVrmlParameter
  else
    if KeyComp('emissiveColor') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func166 :TtkTokenKind;
begin
  if KeyComp('DirectionalLight') then
    Result := tkVrmlLight
  else
    if KeyComp('constructor') then
      Result := tkKey
    else
      if KeyComp('getMilliseconds') then
        Result := tkNonReservedKey
      else
        if KeyComp('toUTCString') then
          Result := tkNonReservedKey
        else
          Result := tkIdentifier;
end;

function TSynVrml97Syn.Func167 :TtkTokenKind;
begin
  if KeyComp('setUTCSeconds') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func168 :TtkTokenKind;
begin
  if KeyComp('GeoElevationGrid') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func169 :TtkTokenKind;
begin
  if KeyComp('defaultStatus') then
    Result := tkNonReservedKey
  else
    if KeyComp('captureEvents') then
      Result := tkNonReservedKey
    else
      if KeyComp('setUTCHours') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func170 :TtkTokenKind;
begin
  if KeyComp('X3DCoordinateNode') then
    Result := tkVrmlProto
  else
    if KeyComp('EXTERNPROTO') then
      Result := tkVrmlProto
    else
      if KeyComp('toLocaleString') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func171 :TtkTokenKind;
begin
  if KeyComp('BooleanSequencer') then
    Result := tkVrmlSensor
  else
    if KeyComp('topToBottom') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func172 :TtkTokenKind;
begin
  if KeyComp('directOutput') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func173 :TtkTokenKind;
begin
  if KeyComp('X3DGroupingNode') then
    Result := tkVrmlProto
  else
    if KeyComp('X3DSequencerNode') then
      Result := tkVrmlProto
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func174 :TtkTokenKind;
begin
  if KeyComp('X3DGeometryNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func175 :TtkTokenKind;
begin
  if KeyComp('duration_changed') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func176 :TtkTokenKind;
begin
  if KeyComp('getUTCFullYear') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func177 :TtkTokenKind;
begin
  if KeyComp('StringSensor') then
    Result := tkVrmlSensor
  else
    if KeyComp('MovieTexture') then
      Result := tkVrmlTime_Dependent
    else
      if KeyComp('getUTCMinutes') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func178 :TtkTokenKind;
begin
  if KeyComp('X3DGeometry2DNode') then
    Result := tkVrmlProto
  else
    if KeyComp('X3DGeometry3DNode') then
      Result := tkVrmlProto
    else
      if KeyComp('setMilliseconds') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func179 :TtkTokenKind;
begin
  if KeyComp('X3DTextureNode') then
    Result := tkVrmlProto
  else
    if KeyComp('PixelTexture') then
      Result := tkVrmlAppearance
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func180 :TtkTokenKind;
begin
  if KeyComp('FillProperties') then
    Result := tkVrmlAttribute
  else
    if KeyComp('scaleOrientation') then
      Result := tkVrmlAttribute
    else
      if KeyComp('CylinderSensor') then
        Result := tkVrmlSensor
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func181 :TtkTokenKind;
begin
  if KeyComp('LineProperties') then
    Result := tkVrmlAttribute
  else
    if KeyComp('PER_FACE_INDEXED') then
      Result := tkVrmlParameter
    else
      if KeyComp('visibilityRange') then
        Result := tkVrmlAttribute
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func182 :TtkTokenKind;
begin
  if KeyComp('X3DAppearanceChildNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func183 :TtkTokenKind;
begin
  if KeyComp('X3DTexture2DNode') then
    Result := tkVrmlProto
  else
    if KeyComp('hitNormal_changed') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func184 :TtkTokenKind;
begin
  if KeyComp('GeoTouchSensor') then
    Result := tkVrmlSensor
  else
    if KeyComp('hitPoint_changed') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func185 :TtkTokenKind;
begin
  if KeyComp('IntegerSequencer') then
    Result := tkVrmlTime_dependent
  else
    if KeyComp('rotation_changed') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func186 :TtkTokenKind;
begin
  if KeyComp('X3DDragSensorNode') then
    Result := tkVrmlProto
  else
    if KeyComp('set_coordIndex') then
      Result := tkVrmlAttribute
    else
      if KeyComp('axisOfRotation') then
        Result := tkVrmlAttribute
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func188 :TtkTokenKind;
begin
  if KeyComp('MultiTexture') then
    Result := tkVrmlAttribute
  else
    if KeyComp('CoordinateDeformer') then
      Result := tkVrmlNode
    else
      if KeyComp('setUTCFullYear') then
        Result := tkNonReservedKey
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func189 :TtkTokenKind;
begin
  if KeyComp('ESPDUTransform') then
    Result := tkVrmlGrouping
  else
    if KeyComp('setUTCMinutes') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func190 :TtkTokenKind;
begin
  if KeyComp('position_changed') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func194 :TtkTokenKind;
begin
  if KeyComp('set_colorIndex') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func196 :TtkTokenKind;
begin
  if KeyComp('COUNTERCLOCKWISE') then
    Result := tkVrmlParameter
  else
    if KeyComp('colorPerVertex') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func198 :TtkTokenKind;
begin
  if KeyComp('TransmitterPdu') then
    Result := tkVrmlGrouping
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func199 :TtkTokenKind;
begin
  if KeyComp('ambientIntensity') then
    Result := tkVrmlAttribute
  else
    if KeyComp('visibilityLimit') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func200 :TtkTokenKind;
begin
  if KeyComp('X3DTimeDependentNode') then
    Result := tkVrmlProto
  else
    if KeyComp('trackPoint_changed') then
      Result := tkVrmlAttribute
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func202 :TtkTokenKind;
begin
  if KeyComp('X3DFontStyleNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func204 :TtkTokenKind;
begin
  if KeyComp('set_normalIndex') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func206 :TtkTokenKind;
begin
  if KeyComp('normalPerVertex') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func209 :TtkTokenKind;
begin
  if KeyComp('TextureBackground') then
    Result := tkVrmlAppearance
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func210 :TtkTokenKind;
begin
  if KeyComp('getTimezoneOffset') then
    Result := tkNonReservedKey
  else
    if KeyComp('getUTCMilliseconds') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func212 :TtkTokenKind;
begin
  if KeyComp('TriangleStripSet') then
    Result := tkVrmlShape
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func213 :TtkTokenKind;
begin
  if KeyComp('orientation_changed') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func214 :TtkTokenKind;
begin
  if KeyComp('hitTexCoord_changed') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func215 :TtkTokenKind;
begin
  if KeyComp('set_orientation') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func216 :TtkTokenKind;
begin
  if KeyComp('translation_changed') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func217 :TtkTokenKind;
begin
  if KeyComp('TextureCoordinate') then
    Result := tkVrmlAppearance
  else
    if KeyComp('ScalarInterpolator') then
      Result := tkVrmlInterpolator
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func218 :TtkTokenKind;
begin
  if KeyComp('ContourPolyline2D') then
    Result := tkVrmlShape
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func220 :TtkTokenKind;
begin
  if KeyComp('X3DSoundSourceNode') then
    Result := tkVrmlProto
  else
    if KeyComp('NEGATIVE_INFINITY') then
      Result := tkNonReservedKey
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func221 :TtkTokenKind;
begin
  if KeyComp('PER_PART_INDEXED') then
    Result := tkVrmlParameter
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func222 :TtkTokenKind;
begin
  if KeyComp('setUTCMilliseconds') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func223 :TtkTokenKind;
begin
  if KeyComp('X3DTouchSensorNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func226 :TtkTokenKind;
begin
  if KeyComp('ColorInterpolator') then
    Result := tkVrmlInterpolator
  else
    if KeyComp('VisibilitySensor') then
      Result := tkVrmlSensor
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func229 :TtkTokenKind;
begin
  if KeyComp('X3DInterpolatorNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func233 :TtkTokenKind;
begin
  if KeyComp('UNKNOWN_ORDERING') then
    Result := tkVrmlParameter
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func234 :TtkTokenKind;
begin
  if KeyComp('set_crossSection') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func235 :TtkTokenKind;
begin
  if KeyComp('set_texCoordIndex') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func236 :TtkTokenKind;
begin
  if KeyComp('NormalInterpolator') then
    Result := tkVrmlInterpolator
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func237 :TtkTokenKind;
begin
  if KeyComp('textureTransform') then
    Result := tkVrmlAttribute
  else
    if KeyComp('TextureTransform') then
      Result := tkVrmlAppearance
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func239 :TtkTokenKind;
begin
  if KeyComp('ProximitySensor') then
    Result := tkVrmlSensor
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func245 :TtkTokenKind;
begin
  if KeyComp('X3DKeyDeviceSensorNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func252 :TtkTokenKind;
begin
  if KeyComp('POSITIVE_INFINITY') then
    Result := tkNonReservedKey
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func255 :TtkTokenKind;
begin
  if KeyComp('UNKNOWN_FACE_TYPE') then
    Result := tkVrmlParameter
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func260 :TtkTokenKind;
begin
  if KeyComp('NurbsTextureSurface') then
    Result := tkVrmlAppearance
  else
    if KeyComp('PER_VERTEX_INDEXED') then
      Result := tkVrmlParameter
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func262 :TtkTokenKind;
begin
  if KeyComp('X3DNetworkSensorNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func263 :TtkTokenKind;
begin
  if KeyComp('X3DPrototypeInstance') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func264 :TtkTokenKind;
begin
  if KeyComp('X3DComposedGeometryNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func267 :TtkTokenKind;
begin
  if KeyComp('CoordinateInterpolator') then
    Result := tkVrmlInterpolator
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func271 :TtkTokenKind;
begin
  if KeyComp('CoordinateInterpolator2D') then
    Result := tkVrmlInterpolator
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func278 :TtkTokenKind;
begin
  if KeyComp('X3DParametricGeometryNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func280 :TtkTokenKind;
begin
  if KeyComp('PositionInterpolator') then
    Result := tkVrmlInterpolator
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func283 :TtkTokenKind;
begin
  if KeyComp('X3DTextureCoordinateNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func284 :TtkTokenKind;
begin
  if KeyComp('PositionInterpolator2D') then
    Result := tkVrmlInterpolator
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func289 :TtkTokenKind;
begin
  if KeyComp('UNKNOWN_SHAPE_TYPE') then
    Result := tkVrmlParameter
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func292 :TtkTokenKind;
begin
  if KeyComp('MultiTextureCoordinate') then
    Result := tkVrmlAttribute
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func303 :TtkTokenKind;
begin
  if KeyComp('X3DTextureTransformNode') then
    Result := tkVrmlProto
  else
    if KeyComp('OrientationInterpolator') then
      Result := tkVrmlInterpolator
    else
      Result := tkIdentifier;
end;

function TSynVrml97Syn.Func307 :TtkTokenKind;
begin
  if KeyComp('GeoPositionInterpolator') then
    Result := tkVrmlInterpolator
  else
    if KeyComp('X3DGeometryPropertyNode') then
      Result := tkVrmlProto
    else
      if KeyComp('X3DTextureTransform2DNode') then
        Result := tkVrmlProto
      else
        Result := tkIdentifier;
end;

function TSynVrml97Syn.Func308 :TtkTokenKind;
begin
  if KeyComp('X3DPointingDeviceSensorNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func318 :TtkTokenKind;
begin
  if KeyComp('X3DEnvironmentalSensorNode') then
    Result := tkVrmlProto
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func320 :TtkTokenKind;
begin
  if KeyComp('TextureCoordinateGenerator') then
    Result := tkVrmlAppearance
  else
    Result := tkIdentifier;
end;

function TSynVrml97Syn.Func354 :TtkTokenKind;
begin
  if KeyComp('NurbsPositionInterpolator') then
    Result := tkVrmlInterpolator
  else
    Result := tkIdentifier;
end;

//------------------------------------------------------------------------------

function TSynVrml97Syn.AltFunc :TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynVrml97Syn.IdentKind(MayBe :PChar) :TtkTokenKind;
var
  HashKey :Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 304 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

constructor TSynVrml97Syn.Create(AOwner :TComponent);
begin
  inherited Create(AOwner);
  isDoctype := false;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  fCommentAttri.Background := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Style := [];
  fIdentifierAttri.Foreground := clNavy;
  fIdentifierAttri.Background := clWhite;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clRed;
  fKeyAttri.Background := clWhite;
  AddAttribute(fKeyAttri);

  fNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword);
  fNonReservedKeyAttri.Style := [fsItalic];
  fNonReservedKeyAttri.Foreground := clBlack;
  fNonReservedKeyAttri.Background := clWhite;
  AddAttribute(fNonReservedKeyAttri);

  fEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent);
  fEventAttri.Style := [fsItalic];
  fEventAttri.Foreground := clNavy;
  fEventAttri.Background := clWhite;
  AddAttribute(fEventAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fEventAttri.Style := [fsItalic];
  fEventAttri.Foreground := clNavy;
  fEventAttri.Background := clWhite;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  fSpaceAttri.Style := [fsItalic];
  fSpaceAttri.Foreground := clNavy;
  fSpaceAttri.Background := clWhite;
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Style := [fsItalic];
  fStringAttri.Foreground := clNavy;
  fStringAttri.Background := clWhite;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Style := [fsItalic];
  fSymbolAttri.Foreground := clNavy;
  fSymbolAttri.Background := clWhite;
  AddAttribute(fSymbolAttri);
  //-- vrml
  fVrmlAppearanceAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlAppearance);
  fVrmlAppearanceAttri.Style := [fsItalic];
  fVrmlAppearanceAttri.Foreground := clNavy;
  fVrmlAppearanceAttri.Background := clWhite;
  AddAttribute(fVrmlAppearanceAttri);

  fVrmlAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlAttribute);
  fVrmlAttributeAttri.Style := [fsItalic];
  fVrmlAttributeAttri.Foreground := clNavy;
  fVrmlAttributeAttri.Background := clGray;
  AddAttribute(fVrmlAttributeAttri);

  fVrmlDefinitionAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlDefinition);
  fVrmlDefinitionAttri.Style := [fsItalic];
  fVrmlDefinitionAttri.Foreground := clNavy;
  fVrmlDefinitionAttri.Background := clRed;
  AddAttribute(fVrmlDefinitionAttri);

  fVrmlEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlEvent);
  fVrmlEventAttri.Style := [fsBold];
  fVrmlEventAttri.Foreground := clRed;
  fVrmlEventAttri.Background := clWhite;
  AddAttribute(fVrmlEventAttri);

  fVrmlGroupingAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlGrouping);
  fVrmlGroupingAttri.Style := [fsBold];
  fVrmlGroupingAttri.Foreground := clNavy;
  fVrmlGroupingAttri.Background := clWhite;
  AddAttribute(fVrmlGroupingAttri);

  fVrmlInterpolatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlInterpolator);
  fVrmlInterpolatorAttri.Style := [fsItalic];
  fVrmlInterpolatorAttri.Foreground := clLime;
  fVrmlInterpolatorAttri.Background := clWhite;
  AddAttribute(fVrmlInterpolatorAttri);

  fVrmlLightAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlLight);
  fVrmlLightAttri.Style := [fsItalic];
  fVrmlLightAttri.Foreground := clTeal;
  fVrmlLightAttri.Background := clWhite;
  AddAttribute(fVrmlLightAttri);

  fVrmlNodeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlNode);
  fVrmlNodeAttri.Style := [fsItalic, fsBold];
  fVrmlNodeAttri.Foreground := clGreen;
  fVrmlNodeAttri.Background := clWhite;
  AddAttribute(fVrmlNodeAttri);

  fVrmlParameterAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlParameter);
  fVrmlParameterAttri.Style := [fsBold];
  fVrmlParameterAttri.Foreground := $F0CAA6; //clSkyBlue
  fVrmlParameterAttri.Background := clWhite;
  AddAttribute(fVrmlParameterAttri);

  fVrmlprotoAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlProto);
  fVrmlprotoAttri.Style := [fsBold];
  fVrmlprotoAttri.Foreground := clRed;
  fVrmlprotoAttri.Background := clWhite;
  AddAttribute(fVrmlprotoAttri);

  fVrmlSensorAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlSensor);
  fVrmlSensorAttri.Style := [fsBold];
  fVrmlSensorAttri.Foreground := clOlive;
  fVrmlSensorAttri.Background := clWhite;
  AddAttribute(fVrmlSensorAttri);

  fVrmlShapeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlShape);
  fVrmlShapeAttri.Style := [fsBold];
  fVrmlShapeAttri.Foreground := clPurple;
  fVrmlShapeAttri.Background := clWhite;
  AddAttribute(fVrmlShapeAttri);

  fVrmlShape_HintAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlShape_Hint);
  fVrmlShape_HintAttri.Style := [fsItalic];
  fVrmlShape_HintAttri.Foreground := clPurple;
  fVrmlShape_HintAttri.Background := clWhite;
  AddAttribute(fVrmlShape_HintAttri);

  fVrmlTime_dependentAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlTime_dependent);
  fVrmlTime_dependentAttri.Style := [fsItalic];
  fVrmlTime_dependentAttri.Foreground := clOlive;
  fVrmlTime_dependentAttri.Background := clWhite;
  AddAttribute(fVrmlTime_dependentAttri);

  fVrmlViewpointAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlViewpoint);
  fVrmlViewpointAttri.Style := [fsItalic];
  fVrmlViewpointAttri.Foreground := clGreen;
  fVrmlViewpointAttri.Background := clWhite;
  AddAttribute(fVrmlViewpointAttri);

  fVrmlWorldInfoAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlWorldInfo);
  fVrmlWorldInfoAttri.Style := [fsItalic];
  fVrmlWorldInfoAttri.Foreground := clMaroon;
  fVrmlWorldInfoAttri.Background := clWhite;
  AddAttribute(fVrmlWorldInfoAttri);

  fX3DDocTypeAttri := TSynHighLighterAttributes.Create(SYNS_AttrX3DDocType);
  fX3DDocTypeAttri.Style := [fsItalic];
  fX3DDocTypeAttri.Foreground := clMaroon;
  fX3DDocTypeAttri.Background := clWhite;
  AddAttribute(fX3DDocTypeAttri);

  fX3DHeaderAttri := TSynHighLighterAttributes.Create(SYNS_AttrX3DHeader);
  fX3DHeaderAttri.Style := [fsItalic];
  fX3DHeaderAttri.Foreground := clMaroon;
  fX3DHeaderAttri.Background := clWhite;
  AddAttribute(fX3DHeaderAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterVrml97;
  fRange := rsNormalText;
end;

procedure TSynVrml97Syn.SetLine(NewValue :string; LineNumber :Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynVrml97Syn.MakeMethodTables;
var
  I :Char;
begin
  for I := #0 to #255 do
    case I of
      '&' :fProcTable[I] := AndSymbolProc;
      #13 :fProcTable[I] := CRProc;
      '#' :fProcTable[I] := DiesisCommentProc;
      'A'..'Z', 'a'..'z', '_' :fProcTable[I] := IdentProc;
      #10 :fProcTable[I] := LFProc;
      '-' :fProcTable[I] := MinusProc;
      '%' :fProcTable[I] := ModSymbolProc;
      #0 :fProcTable[I] := NullProc;
      '0'..'9' :fProcTable[I] := NumberProc;
      '|' :fProcTable[I] := OrSymbolProc;
      '+' :fProcTable[I] := PlusProc;
      '.' :fProcTable[I] := PointProc;
      '/' :fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32 :fProcTable[I] := SpaceProc;
      '*' :fProcTable[I] := StarProc;
      '"', #39 :fProcTable[I] := StringProc;
      '?' :fProcTable[I] := X3DHeaderOpenProc;
      '!' :fProcTable[I] := X3DDocTypeOpenProc;
      '~', '{', '}', ',', '(', ')', '[', ']', ':', ';', '=', '<', '>' :
        fProcTable[I] := SymbolProc;
      else
        fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynVrml97Syn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '&'] then inc(Run);
end;

function TSynVrml97Syn.NextTokenIs(T :string) :Boolean;
var
  I, Len :Integer;
begin
  Result := True;
  Len := Length(T);
  for I := 1 to Len do
    if (fLine[Run + I] <> T[I]) then
      begin
        Result := False;
        Break;
      end;
end;

procedure TSynVrml97Syn.InCommentProc;
begin
  if (fLine[Run + 1] = '-') and (fLine[Run + 2] = '-') then
    begin
      Inc(Run);
      fTokenID := tkComment;
      fRange := rsComment;
      Inc(Run, 2);
      repeat
        Inc(Run);
        if (fLine[Run] = '-') and (fLine[Run + 1] = '-') then
          begin
            fRange := rsNormalText;
            Inc(Run, 2);
            break;
          end;
      until fLine[Run] in [#0, #10, #13];
      Exit;
    end;
end;

procedure TSynVrml97Syn.DiesisCommentProc;
begin
  if fLine[Run] = #0 then
    fTokenID := tkNull
  else
    begin
      fTokenID := tkComment;
      repeat
        inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
end;

procedure TSynVrml97Syn.X3DHeaderOpenProc;
begin
  Inc(Run);
  fRange := rsX3DHeader;
  X3DHeaderProc;
  fTokenID := tkX3DHeader;
end;

procedure TSynVrml97Syn.X3DHeaderProc;
begin
  case fLine[Run] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        fTokenID := tkX3DHeader;
        repeat
          if (fLine[Run] = '?') then
            begin
              Inc(Run, 1);
              fRange := rsNormalText;
              Break;
            end;
          if not (fLine[Run] in [#0, #10, #13]) then
            Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
  end;
end;

procedure TSynVrml97Syn.X3DDocTypeOpenProc;
begin
  if NextTokenIs('DOCTYPE') then
    begin
      fRange := rsX3DDocType;
      X3DDocTypeProc;
      fTokenID := tkX3DDocType;
    end
  else
    if NextTokenIs('--') then
      begin
        fRange := rsComment;
        InCommentProc;
        fTokenID := tkComment;
      end
    else
      fTokenID := tkSymbol;
end;

procedure TSynVrml97Syn.X3DDocTypeProc;
begin
  case fLine[Run] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        fTokenID := tkX3DDocType;
        repeat
          if (fLine[Run + 1] = '>') then
            begin
              Inc(Run, 1);
              fRange := rsNormalText;
              Break;
            end;
          if not (fLine[Run] in [#0, #10, #13]) then
            Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
  end;
end;

procedure TSynVrml97Syn.CommentProc;
begin
  if fLine[Run] = #0 then
    fTokenID := tkNull
  else
    begin
      fTokenID := tkComment;
      repeat
        if ((fLine[Run] = '*') and (fLine[Run + 1] = '/'))
          or
          ((fLine[Run] = '-') and (fLine[Run + 1] = '-')) then
          begin
            fRange := rsNormalText;
            inc(Run, 2);
            break;
          end;
        inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
end;

procedure TSynVrml97Syn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynVrml97Syn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    inc(Run);
end;

procedure TSynVrml97Syn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVrml97Syn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '-', '>'] then inc(Run);
end;

procedure TSynVrml97Syn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynVrml97Syn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynVrml97Syn.NumberProc;
var
  idx1 :Integer; // token[1]
  isHex :Boolean;
begin
  fTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while FLine[Run] in ['0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X'] do
    begin
      case FLine[Run] of
        '.' :
          if FLine[Succ(Run)] = '.' then
            Break;
        'a'..'f', 'A'..'F' :
          if not isHex then
            Break;
        'x', 'X' :
          begin
            if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
              Break;
            if not (FLine[Succ(Run)] in ['0'..'9', 'a'..'f', 'A'..'F']) then
              Break;
            isHex := True;
          end;
      end;
      Inc(Run);
    end;
end;

procedure TSynVrml97Syn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '|'] then inc(Run);
end;

procedure TSynVrml97Syn.PlusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '+'] then inc(Run);
end;

procedure TSynVrml97Syn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then inc(Run, 2);
end;

procedure TSynVrml97Syn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/' :
      begin
        fTokenID := tkComment;
        repeat
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
    '*' :
      begin
        fTokenID := tkComment;
        fRange := rsComment;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
            begin
              fRange := rsNormalText;
              Inc(Run, 2);
              break;
            end;
        until fLine[Run] in [#0, #10, #13];
      end;
    '=' :
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynVrml97Syn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TSynVrml97Syn.StarProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynVrml97Syn.StringProc;
var
  l_strChar :string;
begin
  fTokenID := tkString;
  l_strChar := FLine[Run]; // We could have '"' or #39
  if (FLine[Run + 1] = l_strChar) and (FLine[Run + 2] = l_strChar) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13 :break;
    end;
    inc(Run);
  until (FLine[Run] = l_strChar) and (FLine[Pred(Run)] <> '\');
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynVrml97Syn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVrml97Syn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
    inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynVrml97Syn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsX3DHeader :X3DHeaderProc;
    rsX3DDocType :X3DDocTypeProc;
    rsComment :CommentProc;
    else
      begin
        fProcTable[fLine[Run]];
      end;
  end;
end;

function TSynVrml97Syn.GetDefaultAttribute(Index :integer) :TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT :Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER :Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD :Result := fKeyAttri;
    SYN_ATTR_STRING :Result := fStringAttri;
    SYN_ATTR_WHITESPACE :Result := fSpaceAttri;
    SYN_ATTR_SYMBOL :Result := fSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynVrml97Syn.GetEol :Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynVrml97Syn.GetRange :Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynVrml97Syn.GetToken :string;
var
  Len :LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynVrml97Syn.GetTokenID :TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVrml97Syn.GetTokenAttribute :TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment :Result := fCommentAttri;
    tkIdentifier :Result := fIdentifierAttri;
    tkKey :Result := fKeyAttri;
    tkNonReservedKey :Result := fNonReservedKeyAttri;
    tkEvent :Result := fEventAttri;
    tkNumber :Result := fNumberAttri;
    tkSpace :Result := fSpaceAttri;
    tkString :Result := fStringAttri;
    tkSymbol :Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    // vrml
    tkVrmlAppearance :Result := fVrmlAppearanceAttri;
    tkVrmlAttribute :Result := fVrmlAttributeAttri;
    tkVrmlDefinition :Result := fVrmlDefinitionAttri;
    tkVrmlEvent :Result := fVrmlEventAttri;
    tkVrmlGrouping :Result := fVrmlGroupingAttri;
    tkVrmlInterpolator :Result := fVrmlInterpolatorAttri;
    tkVrmlLight :Result := fVrmlLightAttri;
    tkVrmlNode :Result := fVrmlNodeAttri;
    tkVrmlParameter :Result := fVrmlParameterAttri;
    tkVrmlproto :Result := fVrmlprotoAttri;
    tkVrmlSensor :Result := fVrmlSensorAttri;
    tkVrmlShape :Result := fVrmlShapeAttri;
    tkVrmlShape_Hint :Result := fVrmlShape_HintAttri;
    tkVrmlTime_dependent :Result := fVrmlTime_dependentAttri;
    tkVrmlViewpoint :Result := fVrmlViewpointAttri;
    tkVrmlWorldInfo :Result := fVrmlWorldInfoAttri;
    tkX3DDocType :Result := fX3DDocTypeAttri;
    tkX3DHeader :Result := fX3DHeaderAttri;
    //--
    else
      Result := nil;
  end;
end;

function TSynVrml97Syn.GetTokenKind :integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVrml97Syn.GetTokenPos :Integer;
begin
  Result := fTokenPos;
end;

procedure TSynVrml97Syn.ResetRange;
begin
  fRange := rsNormalText;
end;

procedure TSynVrml97Syn.SetRange(Value :Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynVrml97Syn.GetIdentChars :TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynVrml97Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVrml97;
end;

class function TSynVrml97Syn.GetLanguageName :string;
begin
  Result := SYNS_LangVrml97;
end;

function TSynVrml97Syn.GetSampleSource :string;
begin
  Result :=
    '#VRML V2.0 utf8'#13#10 +
    'Transform {'#13#10 +
    '  children ['#13#10 +
    '    NavigationInfo { headlight FALSE } # We''ll add our own light'#13#10 +
    ''#13#10 +
    '    DirectionalLight {        # First child'#13#10 +
    '        direction 0 0 -1      # Light illuminating the scene'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    Transform {               # Second child - a red sphere'#13#10 +
    '      translation 3 0 1'#13#10 +
    '      children ['#13#10 +
    '        Shape {'#13#10 +
    '          geometry Sphere { radius 2.3 }'#13#10 +
    '          appearance Appearance {'#13#10 +
    '            material Material { diffuseColor 1 0 0 }   # Red'#13#10 +
    '         }'#13#10 +
    '        }'#13#10 +
    '      ]'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    Transform {               # Third child - a blue box '#13#10 +
    '      translation -2.4 .2 1'#13#10 +
    '      rotation     0 1 1  .9'#13#10 +
    '      children ['#13#10 +
    '        Shape {'#13#10 +
    '          geometry Box {}'#13#10 +
    '          appearance Appearance {'#13#10 +
    '            material Material { diffuseColor 0 0 1 }  # Blue'#13#10 +
    '         }'#13#10 +
    '        }'#13#10 +
    '      ]'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '  ] # end of children for world'#13#10 +
    '}'#13#10 +
    'DEF Example_2 Script {'#13#10 +
    '    field   SFNode myself USE Example_2'#13#10 +
    '    field   SFNode root USE ROOT_TRANSFORM'#13#10 +
    '    field   MFString url "foo.wrl"'#13#10 +
    '    eventIn MFNode   nodesLoaded'#13#10 +
    '    eventIn SFBool   trigger_event'#13#10 +
    ''#13#10 +
    '    url "javascript:'#13#10 +
    '        function trigger_event(value, ts){'#13#10 +
    '            // do something and then fetch values'#13#10 +
    '            Browser.createVRMLFromURL(url, myself, ''nodesLoaded'');'#13#10 +
    '        }'#13#10 +
    ''#13#10 +
    '        function nodesLoaded(value, timestamp){'#13#10 +
    '            if (value.length > 5) {'#13#10 +
    '                 // do something more than 5 nodes in this MFNode...'#13#10 +
    '            }'#13#10 +
    '            root.addChildren = value;'#13#10 +
    '        }"'#13#10 +
    '}';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynVrml97Syn);
{$ENDIF}
end.
