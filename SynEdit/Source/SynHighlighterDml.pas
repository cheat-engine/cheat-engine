{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterDml.pas, released 2000-04-17.
The Original Code is based on the mwDmlSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Peter Adam.
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

$Id: SynHighlighterDml.pas,v 1.12 2005/01/28 16:53:22 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - There are no metadata qualifiers.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Dml highlighter for SynEdit)
@author(Peter Adam)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterDml unit provides SynEdit with a Dml highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERDML}
unit SynHighlighterDml;
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
  TtkTokenKind = (tkBlock, tkComment, tkForm, tkFunction, tkIdentifier, tkKey,
    tkNull, tkNumber, tkQualifier, tkSpace, tkSpecial, tkString, tkSymbol,
    tkUnknown, tkVariable);

  TRangeState = (rsANil, rsAdd, rsFind, rsUnKnown);

  TProcTableProc = procedure of object;
  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynDmlSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    Temp: PChar;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..327] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fFormAttri: TSynHighlighterAttributes;
    fBlockAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fQualiAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fSpecialAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func9: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func142: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func163: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func175: TtkTokenKind;
    function Func176: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func179: TtkTokenKind;
    function Func182: TtkTokenKind;
    function Func183: TtkTokenKind;
    function Func184: TtkTokenKind;
    function Func185: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func203: TtkTokenKind;
    function Func206: TtkTokenKind;
    function Func216: TtkTokenKind;
    function Func219: TtkTokenKind;
    function Func221: TtkTokenKind;
    function Func232: TtkTokenKind;
    function Func234: TtkTokenKind;
    function Func235: TtkTokenKind;
    function Func243: TtkTokenKind;
    function Func244: TtkTokenKind;
    function Func255: TtkTokenKind;
    function Func313: TtkTokenKind;
    function Func327: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure SymbolProc;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure RemProc;
    function IsQuali: boolean;
    function IsSpecial: Boolean;
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
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property BlockAttri: TSynHighlighterAttributes read fBlockAttri
      write fBlockAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property FormAttri: TSynHighlighterAttributes read fFormAttri
      write fFormAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property QualiAttri: TSynHighlighterAttributes read fQualiAttri
      write fQualiAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SpecialAttri: TSynHighlighterAttributes read fSpecialAttri
      write fSpecialAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
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
    else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I of
      'a'..'z', 'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
    else mHashTable[Char(I)] := 0;
    end;
  end;
end;

procedure TSynDmlSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[9] := Func9;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[24] := Func24;
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
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[62] := Func62;
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
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[123] := Func123;
  fIdentFuncTable[124] := Func124;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[132] := Func132;
  fIdentFuncTable[134] := Func134;
  fIdentFuncTable[135] := Func135;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[137] := Func137;
  fIdentFuncTable[138] := Func138;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[140] := Func140;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[142] := Func142;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[148] := Func148;
  fIdentFuncTable[150] := Func150;
  fIdentFuncTable[152] := Func152;
  fIdentFuncTable[153] := Func153;
  fIdentFuncTable[154] := Func154;
  fIdentFuncTable[155] := Func155;
  fIdentFuncTable[156] := Func156;
  fIdentFuncTable[157] := Func157;
  fIdentFuncTable[163] := Func163;
  fIdentFuncTable[164] := Func164;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[169] := Func169;
  fIdentFuncTable[173] := Func173;
  fIdentFuncTable[174] := Func174;
  fIdentFuncTable[175] := Func175;
  fIdentFuncTable[176] := Func176;
  fIdentFuncTable[178] := Func178;
  fIdentFuncTable[179] := Func179;
  fIdentFuncTable[182] := Func182;
  fIdentFuncTable[183] := Func183;
  fIdentFuncTable[184] := Func184;
  fIdentFuncTable[185] := Func185;
  fIdentFuncTable[187] := Func187;
  fIdentFuncTable[188] := Func188;
  fIdentFuncTable[203] := Func203;
  fIdentFuncTable[206] := Func206;
  fIdentFuncTable[216] := Func216;
  fIdentFuncTable[219] := Func219;
  fIdentFuncTable[221] := Func221;
  fIdentFuncTable[232] := Func232;
  fIdentFuncTable[234] := Func234;
  fIdentFuncTable[235] := Func235;
  fIdentFuncTable[243] := Func243;
  fIdentFuncTable[244] := Func244;
  fIdentFuncTable[255] := Func255;
  fIdentFuncTable[313] := Func313;
  fIdentFuncTable[327] := Func327;
end;

function TSynDmlSyn.IsQuali: boolean;
begin
  Result:= False;
  if Run > 0 then
    if fLine[Run-1]= '/' then Result:= True;
end;

function TSynDmlSyn.IsSpecial: boolean;
begin
  Result:= False;
  if Run > 0 then
    if fLine[Run-1]= '%' then Result:= True;
end;

function TSynDmlSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z', '_'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then inc(ToHash);
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynDmlSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
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

function TSynDmlSyn.Func9: TtkTokenKind;
begin
  if KeyComp('Add') then
  begin
    if IsSpecial then Result := tkSpecial
    else begin
      Result := tkKey;
      fRange := rsAdd;
    end;
  end else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func15: TtkTokenKind;
begin
  if KeyComp('If') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func17: TtkTokenKind;
begin
  if KeyComp('Back') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func19: TtkTokenKind;
begin
  if KeyComp('Dcl') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func22: TtkTokenKind;
begin
  if KeyComp('Abs') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func23: TtkTokenKind;
begin
  if KeyComp('In') and (fRange = rsFind) then
  begin
    Result := tkKey;
    fRange:= rsUnKnown;
  end else Result := tkIdentifier;
end;

function TSynDmlSyn.Func24: TtkTokenKind;
begin
  if KeyComp('Cli') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func26: TtkTokenKind;
begin
  if KeyComp('Mid') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func27: TtkTokenKind;
begin
  if KeyComp('Base') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func28: TtkTokenKind;
begin
  if KeyComp('Tag') and IsQuali then Result := tkQualifier else
    if KeyComp('Case') then Result := tkKey else
      if KeyComp('Call') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func29: TtkTokenKind;
begin
  if KeyComp('Chr') then Result := tkFunction else
    if KeyComp('Ceil') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func30: TtkTokenKind;
begin
  if KeyComp('Check') and IsQuali then Result := tkQualifier else
    if KeyComp('Col') then begin
      if IsQuali then Result := tkQualifier else
        if IsSpecial then Result := tkSpecial else Result := tkIdentifier;
    end else
      if KeyComp('Date') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func31: TtkTokenKind;
begin
  if KeyComp('Len') then begin
    if IsQuali then Result := tkQualifier else Result := tkFunction;
  end else
    if KeyComp('Dir') then Result := tkKey else
      if KeyComp('Bell') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func32: TtkTokenKind;
begin
  if KeyComp('Mod') then Result := tkFunction else
    if KeyComp('Load') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func33: TtkTokenKind;
begin
  if KeyComp('Find') then
  begin
    Result := tkKey;
    fRange := rsFind;
  end else Result := tkIdentifier;
end;

function TSynDmlSyn.Func34: TtkTokenKind;
begin
  if KeyComp('Log') then begin
    if IsQuali then Result := tkQualifier else Result := tkFunction;
  end else if KeyComp('Batch') and IsQuali then Result := tkQualifier else
    if KeyComp('Log10') then Result := tkFunction else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func35: TtkTokenKind;
begin
  if KeyComp('Tan') then Result := tkFunction else
    if KeyComp('To') and (fRange=rsAdd) then
    begin
      Result := tkKey;
      fRange := rsUnKnown;
    end else
      if KeyComp('Mail') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func36: TtkTokenKind;
begin
  if KeyComp('Atan2') then Result := tkFunction else
    if KeyComp('Atan') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func37: TtkTokenKind;
begin
  if KeyComp('Break') and IsQuali then Result := tkQualifier else
    if KeyComp('Break0') and IsQuali then Result := tkQualifier else
      if KeyComp('Cos') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func38: TtkTokenKind;
begin
  if KeyComp('Acos') then Result := tkFunction else
    if KeyComp('Edit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func40: TtkTokenKind;
begin
  if KeyComp('Line') then Result := tkKey else
    if KeyComp('Table') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func41: TtkTokenKind;
begin
  if KeyComp('Else') then Result := tkKey else
    if KeyComp('Lock') and IsQuali then Result := tkQualifier else
      if KeyComp('Ascii') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func42: TtkTokenKind;
begin
  if KeyComp('Send') then Result := tkKey else
    if KeyComp('Sin') then Result := tkFunction else
      if KeyComp('New') and IsQuali then Result := tkQualifier else
        if KeyComp('Fetch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func43: TtkTokenKind;
begin
  if KeyComp('Asin') then Result := tkFunction else
    if KeyComp('Int') then Result := tkFunction else
      if KeyComp('Left') then Result := tkFunction else
        if KeyComp('Tanh') then Result := tkFunction else
          Result := tkIdentifier;
end;

function TSynDmlSyn.Func45: TtkTokenKind;
begin
  if KeyComp('Cosh') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func47: TtkTokenKind;
begin
  if KeyComp('Item') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func48: TtkTokenKind;
begin
  if KeyComp('Heading') and IsQuali then Result := tkQualifier else
    if KeyComp('Erase') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func49: TtkTokenKind;
begin
  if KeyComp('Days') then Result := tkFunction else
    if KeyComp('Lov') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func50: TtkTokenKind;
begin
  if KeyComp('Pos') and IsQuali then Result := tkQualifier else
    if KeyComp('Sinh') then Result := tkFunction else
      if KeyComp('Open') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func51: TtkTokenKind;
begin
  if KeyComp('Files') then Result := tkKey else
    if KeyComp('Opt') and IsQuali then Result := tkQualifier else
      if KeyComp('Delete') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func52: TtkTokenKind;
begin
  if KeyComp('Form') then begin
    if IsSpecial then Result := tkSpecial else Result := tkForm;
  end else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func53: TtkTokenKind;
begin
  if KeyComp('Wait') and IsQuali then Result := tkQualifier else
    if KeyComp('Menu') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func54: TtkTokenKind;
begin
  if KeyComp('Close') then Result := tkKey else
    if KeyComp('Search') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func56: TtkTokenKind;
begin
  if KeyComp('Domain') and IsQuali then Result := tkQualifier else
    if KeyComp('Row') and IsQuali then Result := tkQualifier else
      if KeyComp('Row') and IsSpecial then Result := tkSpecial else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func57: TtkTokenKind;
begin
  if KeyComp('While') then Result := tkKey else
    if KeyComp('Height') and IsQuali then Result := tkQualifier else
      if KeyComp('Goto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func58: TtkTokenKind;
begin
  if KeyComp('Exit') and IsQuali then Result := tkQualifier else
    if KeyComp('Exit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func60: TtkTokenKind;
begin
  if KeyComp('List') then Result := tkKey else
    if KeyComp('With') and IsQuali then Result := tkQualifier else
      if KeyComp('Trim') then Result := tkFunction else
        if KeyComp('Nobell') and IsQuali then Result := tkFunction else
          if KeyComp('Lheading') and IsQuali then Result := tkQualifier else
            if KeyComp('Remain') and IsQuali then Result := tkQualifier else
              Result := tkIdentifier;
end;

function TSynDmlSyn.Func62: TtkTokenKind;
begin
  if KeyComp('Right') then Result := tkFunction else
    if KeyComp('Pause') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func64: TtkTokenKind;
begin
  if KeyComp('Width') and IsQuali then Result := tkQualifier else
    if KeyComp('Expand') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func65: TtkTokenKind;
begin
  if KeyComp('Finish') then Result := tkKey else
    if KeyComp('Release') then Result := tkKey else
      if KeyComp('Random') then Result := tkKey else
        if KeyComp('Repeat') then begin
          if IsQuali then Result := tkQualifier else Result := tkKey;
        end else
          Result := tkIdentifier;
end;

function TSynDmlSyn.Func66: TtkTokenKind;
begin
  if KeyComp('Rheading') and IsQuali then Result := tkQualifier else
    if KeyComp('Floor') then Result := tkKey else
      if KeyComp('Title') then begin
        if IsQuali then Result := tkQualifier else Result := tkKey;
      end else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func67: TtkTokenKind;
begin
  if KeyComp('Unload') then Result := tkKey else
    if KeyComp('Receive') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func68: TtkTokenKind;
begin
  if KeyComp('Total') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func69: TtkTokenKind;
begin
  if KeyComp('Message') then Result := tkKey else
    if KeyComp('End_if') then Result := tkKey else
      if KeyComp('Text') then begin
        if IsSpecial then Result := tkSpecial else Result := tkKey;
      end else if KeyComp('End_if') then Result := tkKey else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func70: TtkTokenKind;
begin
  if KeyComp('Using') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func71: TtkTokenKind;
begin
  if KeyComp('Target') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func72: TtkTokenKind;
begin
  if KeyComp('First') and IsQuali then Result := tkQualifier else
    if KeyComp('Failure') then begin
      if IsQuali then Result := tkQualifier else
        if IsSpecial then Result := tkSpecial else Result := tkIdentifier;
    end else
      if KeyComp('Ltrim') then Result := tkFunction else
        if KeyComp('Round') then Result := tkFunction else
          Result := tkIdentifier;
end;

function TSynDmlSyn.Func73: TtkTokenKind;
begin
  if KeyComp('Commit') then Result := tkKey else
    if KeyComp('Compile') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func74: TtkTokenKind;
begin
  if KeyComp('Sqrt') then Result := tkFunction else
    if KeyComp('Error') then begin
      if IsQuali then Result := tkQualifier else Result := tkKey;
    end else
      if KeyComp('Rollback') then Result := tkKey else
        if KeyComp('Connect') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func75: TtkTokenKind;
begin
  if KeyComp('Generate') then Result := tkKey else
    if KeyComp('Write') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func76: TtkTokenKind;
begin
  if KeyComp('Invoke') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func77: TtkTokenKind;
begin
  if KeyComp('Noerase') and IsQuali then Result := tkQualifier else
   if KeyComp('Noheading') and IsQuali then Result := tkQualifier else
    if KeyComp('Account') and IsSpecial then Result := tkSpecial else
      if KeyComp('Print') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func78: TtkTokenKind;
begin
  if KeyComp('Confirm') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func79: TtkTokenKind;
begin
  if KeyComp('Seconds') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynDmlSyn.Func81: TtkTokenKind;
begin
  if KeyComp('Source') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func82: TtkTokenKind;
begin
  if KeyComp('End_case') then Result := tkKey else
    if KeyComp('Switch') and IsQuali then Result := tkQualifier else
      if KeyComp('Nowait') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func83: TtkTokenKind;
begin
  if KeyComp('Execute') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func84: TtkTokenKind;
begin
  if KeyComp('Trigger') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func85: TtkTokenKind;
begin
  if KeyComp('Facility') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func86: TtkTokenKind;
begin
  if KeyComp('Footing') and IsQuali then Result := tkQualifier else
    if KeyComp('Query') then Result := tkKey else
      if KeyComp('Display') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func87: TtkTokenKind;
begin
  if KeyComp('String') then Result := tkFunction else
    if KeyComp('Else_if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func89: TtkTokenKind;
begin
  if KeyComp('Sequence') and IsQuali then Result := tkQualifier else
    if KeyComp('Success') then begin
      if IsQuali then Result := tkQualifier else
        if IsSpecial then Result := tkSpecial else Result := tkIdentifier;
    end else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func91: TtkTokenKind;
begin
  if KeyComp('Perform') then Result := tkKey else
    if KeyComp('Use_if') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func92: TtkTokenKind;
begin
  if KeyComp('Report') then Result := tkKey else
    if KeyComp('Add_form') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func93: TtkTokenKind;
begin
  if KeyComp('Item_if') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func94: TtkTokenKind;
begin
  if KeyComp('Norepeat') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func96: TtkTokenKind;
begin
  if KeyComp('Begin_case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func97: TtkTokenKind;
begin
  if KeyComp('Protect') and IsQuali then Result := tkQualifier else
    if KeyComp('End_block') then Result := tkBlock else Result := tkIdentifier;
end;

function TSynDmlSyn.Func98: TtkTokenKind;
begin
  if KeyComp('Lfooting') and IsQuali then Result := tkQualifier else
    if KeyComp('Prompt') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func99: TtkTokenKind;
begin
  if KeyComp('Identifier') and IsQuali then Result := tkQualifier else
    if KeyComp('Send_data') then Result := tkKey else
      if KeyComp('Read_line') then Result := tkKey else
        if KeyComp('External') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func100: TtkTokenKind;
begin
  if KeyComp('Status') then begin
    if IsQuali then Result := tkQualifier else
      if IsSpecial then Result := tkSpecial else Result := tkIdentifier;
  end else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func101: TtkTokenKind;
begin
  if KeyComp('Continue') and IsQuali then Result := tkQualifier else
    if KeyComp('System') and IsQuali then Result := tkQualifier else
      if KeyComp('Transfer') then Result := tkKey else
        if KeyComp('Lowercase') then Result := tkFunction else
          Result := tkIdentifier;
end;

function TSynDmlSyn.Func102: TtkTokenKind;
begin
  if KeyComp('Selection') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func103: TtkTokenKind;
begin
  if KeyComp('Noerror') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func104: TtkTokenKind;
begin
  if KeyComp('Secondary') and IsQuali then Result := tkQualifier else
    if KeyComp('Uppercase') then Result := tkFunction else
      if KeyComp('Rfooting') then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func106: TtkTokenKind;
begin
  if KeyComp('End_form') then Result := tkForm else
    if KeyComp('Lov_data') and IsQuali then Result := tkQualifier else
      if KeyComp('Disconnect') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func108: TtkTokenKind;
begin
  if KeyComp('Options') and IsQuali then Result := tkQualifier else
    if KeyComp('Compress') then Result := tkFunction else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func110: TtkTokenKind;
begin
  if KeyComp('End_row') and IsQuali then Result := tkQualifier else
    if KeyComp('Lov_col') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func111: TtkTokenKind;
begin
  if KeyComp('End_while') then Result := tkKey else
    if KeyComp('Begin_block') then Result := tkBlock else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func113: TtkTokenKind;
begin
  if KeyComp('Send_table') then Result := tkKey else
    if KeyComp('Output') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func116: TtkTokenKind;
begin
  if KeyComp('Find_form') and IsQuali then Result := tkQualifier else
    if KeyComp('Nototals') and IsSpecial then Result := tkSpecial else
      if KeyComp('No_domain') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func117: TtkTokenKind;
begin
  if KeyComp('Check_domain') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func120: TtkTokenKind;
begin
  if KeyComp('Statistic') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func121: TtkTokenKind;
begin
  if KeyComp('Item_block') then Result := tkBlock else Result := tkIdentifier;
end;

function TSynDmlSyn.Func122: TtkTokenKind;
begin
  if KeyComp('Top_line') and IsSpecial then Result := tkSpecial else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func123: TtkTokenKind;
begin
  if KeyComp('Severity') and IsQuali then Result := tkQualifier else
    if KeyComp('Joined_to') and IsQuali then Result := tkQualifier else
      if KeyComp('Table_form') then Result := tkForm else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func124: TtkTokenKind;
begin
  if KeyComp('Begin_row') and IsQuali then Result := tkQualifier else
    if KeyComp('Utilities') then Result := tkKey else
      if KeyComp('Receive_data') then Result := tkKey else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func125: TtkTokenKind;
begin
  if KeyComp('Read_only') and IsQuali then Result := tkQualifier else
    if KeyComp('Table_search') then Result := tkKey else
      if KeyComp('Tag_length') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func126: TtkTokenKind;
begin
  if KeyComp('Reduced_to') and IsQuali then Result := tkQualifier else
    if KeyComp('Actual_break') and IsSpecial then Result := tkSpecial else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func127: TtkTokenKind;
begin
  if KeyComp('Source_if') and IsQuali then Result := tkQualifier else
    if KeyComp('Menu_block') then Result := tkBlock else Result := tkIdentifier;
end;

function TSynDmlSyn.Func128: TtkTokenKind;
begin
  if KeyComp('Clear_buffer') then Result := tkKey else
    if KeyComp('Default_tag') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func129: TtkTokenKind;
begin
  if KeyComp('Nostatus') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func131: TtkTokenKind;
begin
  if KeyComp('Heading_form') then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func132: TtkTokenKind;
begin
  if KeyComp('Description') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func134: TtkTokenKind;
begin
  if KeyComp('Delete_form') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func135: TtkTokenKind;
begin
  if KeyComp('Nolov_data') and IsQuali then Result := tkQualifier else
    if KeyComp('Attributes') and IsQuali then Result := tkQualifier else
      if KeyComp('User_key') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func136: TtkTokenKind;
begin
  if KeyComp('Menu_form') then Result := tkForm else
    if KeyComp('Pause_block') then Result := tkBlock else
      if KeyComp('Lov_row') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func137: TtkTokenKind;
begin
  if KeyComp('Lov_height') and IsQuali then Result := tkQualifier else
    if KeyComp('End_execute') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func138: TtkTokenKind;
begin
  if KeyComp('Receive_table') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func139: TtkTokenKind;
begin
  if KeyComp('Sorted_by') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func140: TtkTokenKind;
begin
  if KeyComp('Date_seconds') then Result := tkFunction else
    if KeyComp('Reposition') then Result := tkKey else
      if KeyComp('Switch_base') and IsQuali then Result := tkQualifier else
        if KeyComp('Lines_after') and IsQuali then Result := tkQualifier else
          if KeyComp('Lov_with') and IsQuali then Result := tkQualifier else
            if KeyComp('Lines_after') and IsSpecial then Result := tkSpecial
              else if KeyComp('Stream_name') and IsQuali then
                Result := tkQualifier
              else Result := tkIdentifier;
end;

function TSynDmlSyn.Func141: TtkTokenKind;
begin
  if KeyComp('Lines_before') and IsQuali then Result := tkQualifier else
    if KeyComp('Lines_after') and IsSpecial then Result := tkSpecial else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func142: TtkTokenKind;
begin
  if KeyComp('Send_message') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func144: TtkTokenKind;
begin
  if KeyComp('Grouped_by') and IsQuali then Result := tkQualifier else
    if KeyComp('Lov_width') and IsQuali then Result := tkQualifier else
      if KeyComp('Row_height') and IsQuali then Result := tkQualifier else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func146: TtkTokenKind;
begin
  if KeyComp('Write_line') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func148: TtkTokenKind;
begin
  if KeyComp('Commit_rate') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func150: TtkTokenKind;
begin
  if KeyComp('Open_text') then Result := tkKey else
    if KeyComp('Nounderlines') then begin
      if IsQuali then Result := tkQualifier else
        if IsSpecial then Result := tkSpecial else Result := tkIdentifier;
    end else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func152: TtkTokenKind;
begin
  if KeyComp('Lov_first') and IsQuali then Result := tkQualifier else
    if KeyComp('Yesno_block') then Result := tkBlock else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func153: TtkTokenKind;
begin
  if KeyComp('Tsuppress') and IsSpecial then Result := tkSpecial else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func154: TtkTokenKind;
begin
  if KeyComp('Documentation') then Result := tkKey else
    if KeyComp('Input_block') then Result := tkBlock else
      if KeyComp('Close_text') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func155: TtkTokenKind;
begin
  if KeyComp('Modify_form') and IsQuali then Result := tkQualifier else
    if KeyComp('Input_mask') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func156: TtkTokenKind;
begin
  if KeyComp('Bottom_line') and IsSpecial then Result := tkSpecial else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func157: TtkTokenKind;
begin
  if KeyComp('Lov_noheading') and IsQuali then Result := tkQualifier else
    if KeyComp('Noclear_buffer') and IsQuali then Result := tkQualifier else
      if KeyComp('Day_of_week') then Result := tkFunction else
        Result := tkIdentifier;
end;

function TSynDmlSyn.Func163: TtkTokenKind;
begin
  if KeyComp('Lov_nosearch') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func164: TtkTokenKind;
begin
  if KeyComp('Compress_all') then Result := tkFunction else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func166: TtkTokenKind;
begin
  if KeyComp('Text_only') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func169: TtkTokenKind;
begin
  if KeyComp('Query_form') then Result := tkForm else
    if KeyComp('Footing_form') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func173: TtkTokenKind;
begin
  if KeyComp('Nodeadlock_exit') and IsQuali then Result := tkQualifier else
    if KeyComp('Rewind_text') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func174: TtkTokenKind;
begin
  if KeyComp('Exit_forward') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func175: TtkTokenKind;
begin
  if KeyComp('Report_form') then Result := tkForm else Result := tkIdentifier;
end;

function TSynDmlSyn.Func176: TtkTokenKind;
begin
  if KeyComp('Column_headings') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func178: TtkTokenKind;
begin
  if KeyComp('Column_spacing') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func179: TtkTokenKind;
begin
  if KeyComp('Alternate_form') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func182: TtkTokenKind;
begin
  if KeyComp('Lov_selection') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func183: TtkTokenKind;
begin
  if KeyComp('Display_length') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func184: TtkTokenKind;
begin
  if KeyComp('Lov_secondary') and IsQuali then Result := tkQualifier else
    if KeyComp('Cross_reference') then Result := tkKey else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func185: TtkTokenKind;
begin
  if KeyComp('Start_stream') then Result := tkKey else Result := tkIdentifier;
end;

function TSynDmlSyn.Func187: TtkTokenKind;
begin
  if KeyComp('Output_block') then Result := tkBlock else Result := tkIdentifier;
end;

function TSynDmlSyn.Func188: TtkTokenKind;
begin
  if KeyComp('Output_mask') and IsQuali then Result := tkQualifier else
    if KeyComp('Procedure_form') then Result := tkForm else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func203: TtkTokenKind;
begin
  if KeyComp('Noexit_forward') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func206: TtkTokenKind;
begin
  if KeyComp('Lov_reduced_to') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func216: TtkTokenKind;
begin
  if KeyComp('Receive_arguments') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func219: TtkTokenKind;
begin
  if KeyComp('Lov_sorted_by') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func221: TtkTokenKind;
begin
  if KeyComp('End_disable_trigger') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func232: TtkTokenKind;
begin
  if KeyComp('Lov_auto_select') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func234: TtkTokenKind;
begin
  if KeyComp('Binary_to_poly') then Result := tkFunction else
    if KeyComp('Poly_to_binary') then Result := tkFunction else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func235: TtkTokenKind;
begin
  if KeyComp('Begin_disable_trigger') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func243: TtkTokenKind;
begin
  if KeyComp('Start_transaction') then Result := tkKey else
    if KeyComp('Absolute_position') and IsQuali then Result := tkQualifier else
      Result := tkIdentifier;
end;

function TSynDmlSyn.Func244: TtkTokenKind;
begin
  if KeyComp('Column_heading_row') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func255: TtkTokenKind;
begin
  if KeyComp('Input_row_height') and IsQuali then Result := tkQualifier else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func313: TtkTokenKind;
begin
  if KeyComp('End_signal_to_status') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynDmlSyn.Func327: TtkTokenKind;
begin
  if KeyComp('Begin_signal_to_status') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynDmlSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynDmlSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 328 then Result := fIdentFuncTable[HashKey] else
    Result := tkIdentifier;
end;

procedure TSynDmlSyn.MakeMethodTables;
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
      '"': fProcTable[I] := StringProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := SymbolProc;
      '}': fProcTable[I] := SymbolProc;
      '!': fProcTable[I] := RemProc;
      '.': fProcTable[I] := PointProc;
      '<': fProcTable[I] := LowerProc;
      '>': fProcTable[I] := GreaterProc;
      '@': fProcTable[I] := AddressOpProc;
      #39, '&', '('..'-', '/', ':', ';', '=', '?', '['..'^', '`', '~':
        fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynDmlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fFormAttri:= TSynHighlighterAttributes.Create(SYNS_AttrForm);
  fFormAttri.Style:= [fsBold];
  fFormAttri.Foreground:= clBlue;
  AddAttribute(fFormAttri);
  fBlockAttri:= TSynHighlighterAttributes.Create(SYNS_AttrBlock);
  fBlockAttri.Style:= [fsBold];
  fBlockAttri.Foreground:= clGreen;
  AddAttribute(fBlockAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsBold];
  fCommentAttri.Foreground:= clRed;
  AddAttribute(fCommentAttri);
  fQualiAttri:= TSynHighlighterAttributes.Create(SYNS_AttrQualifier);
  fQualiAttri.Style:= [fsItalic];
  fQualiAttri.Foreground:= clGreen;
  AddAttribute(fQualiAttri);
  fFunctionAttri:= TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  fFunctionAttri.Style:= [fsItalic];
  fFunctionAttri.Foreground:= clBlack;
  AddAttribute(fFunctionAttri);
  fVariableAttri:= TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  fVariableAttri.Style:= [fsBold, fsItalic];
  fVariableAttri.Foreground:= clBlack;
  AddAttribute(fVariableAttri);
  fSpecialAttri:= TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable);
  fSpecialAttri.Style:= [fsItalic];
  fSpecialAttri.Foreground:= clBlack;
  AddAttribute(fSpecialAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
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

  fDefaultFilter := SYNS_FilterGembase;
end;

procedure TSynDmlSyn.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynDmlSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '@' then Inc(Run);
end;

procedure TSynDmlSyn.AsciiCharProc;
begin
  // variables...
  fTokenID := tkVariable;
  repeat
    inc(Run);
  until not (FLine[Run] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']);
end;

procedure TSynDmlSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynDmlSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if FLine[Run] = #10 then inc(Run);
end;

procedure TSynDmlSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynDmlSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynDmlSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynDmlSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run]= '=') or (fLine[Run]= '>') then Inc(Run);
end;

procedure TSynDmlSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynDmlSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynDmlSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run]='.') or (fLine[Run]=')') then inc(Run);
end;

procedure TSynDmlSyn.RemProc;
var
  p: PChar;
begin
  p := PChar(@fLine[Run - 1]);
  while p >= fLine do begin
    if not (p^ in [#9, #32]) then begin
      inc(Run);
      fTokenID := tkSymbol;
      exit;
    end;
    Dec(p);
  end;
  // it is a comment...
  fTokenID := tkComment;
  p := PChar(@fLine[Run]);
  repeat
    Inc(p);
  until p^ in [#0, #10, #13];
  Run := p - fLine;
end;

procedure TSynDmlSyn.SpaceProc;
var p: PChar;
begin
  fTokenID := tkSpace;
  p := PChar(@fLine[Run]);
  repeat
    Inc(p);
  until (p^ > #32) or (p^ in [#0, #10, #13]);
  Run := p - fLine;
end;

procedure TSynDmlSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then inc(Run, 2);
  repeat
    inc(Run);
  until (FLine[Run] in ['"', #0, #10, #13]);

  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynDmlSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDmlSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynDmlSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynDmlSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynDmlSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynDmlSyn.GetTokenID: TtkTokenKind;
begin
  Result:= fTokenId;
end;

function TSynDmlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkForm: Result := fFormAttri;
    tkBlock: Result := fBlockAttri;
    tkKey: Result := fKeyAttri;
    tkComment: Result := fCommentAttri;
    tkQualifier: Result := fQualiAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkNumber: Result := fNumberAttri;
    tkSpecial: Result := fSpecialAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkVariable: Result := fVariableAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
    else Result := nil;
  end;
end;

function TSynDmlSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynDmlSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynDmlSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynDmlSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynDmlSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

function TSynDmlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGembase;
end;

class function TSynDmlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGembase;
end;

function TSynDmlSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynDmlSyn);
{$ENDIF}
end.
