{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCPM.pas, released 2001-08-14.
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

$Id: SynHighlighterCPM.pas,v 1.17 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERCPM}
unit SynHighlighterCPM;
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

const
  MaxKey = 291;

Type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkSQLKey,
    tkString,
    tkSymbol,
    tkSpecialVar,
    tkSystem,
    tkVariable,
    tkNumber,
    tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TRangeState = (rsBraceComment, rsUnKnown);

type
  TSynCPMSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fRange: TRangeState;
    fCommentLevel: Integer;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..MaxKey] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSQLKeyAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSpecialVarAttri: TSynHighlighterAttributes;
    fSystemAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func15: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func142: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func147: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func162: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func165: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func170: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func186: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func198: TtkTokenKind;
    function Func210: TtkTokenKind;
    function Func211: TtkTokenKind;
    function Func212: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func271: TtkTokenKind;
    function Func273: TtkTokenKind;
    function Func291: TtkTokenKind;
    procedure CRProc;
    procedure LFProc;
    procedure SemiColonProc;
    procedure SymbolProc;
    procedure NumberProc;                                                    
    procedure BraceOpenProc;
    procedure IdentProc;
    procedure VariableProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure BraceCommentProc;
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
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SQLKeyAttri: TSynHighlighterAttributes read fSQLKeyAttri write fSQLKeyAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property SpecialVarAttri: TSynHighlighterAttributes read fSpecialVarAttri write fSpecialVarAttri;
    property SystemAttri: TSynHighlighterAttributes read fSystemAttri write fSystemAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri write fVariableAttri;
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
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True : mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end; { MakeIdentTable }


procedure TSynCPMSyn.InitIdent;
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
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[122] := Func122;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[134] := Func134;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[137] := Func137;
  fIdentFuncTable[138] := Func138;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[142] := Func142;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[146] := Func146;
  fIdentFuncTable[147] := Func147;
  fIdentFuncTable[148] := Func148;
  fIdentFuncTable[149] := Func149;
  fIdentFuncTable[153] := Func153;
  fIdentFuncTable[154] := Func154;
  fIdentFuncTable[156] := Func156;
  fIdentFuncTable[157] := Func157;
  fIdentFuncTable[160] := Func160;
  fIdentFuncTable[162] := Func162;
  fIdentFuncTable[164] := Func164;
  fIdentFuncTable[165] := Func165;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[170] := Func170;
  fIdentFuncTable[174] := Func174;
  fIdentFuncTable[178] := Func178;
  fIdentFuncTable[186] := Func186;
  fIdentFuncTable[187] := Func187;
  fIdentFuncTable[188] := Func188;
  fIdentFuncTable[198] := Func198;
  fIdentFuncTable[210] := Func210;
  fIdentFuncTable[211] := Func211;
  fIdentFuncTable[212] := Func212;
  fIdentFuncTable[213] := Func213;
  fIdentFuncTable[271] := Func271;
  fIdentFuncTable[273] := Func273;
  fIdentFuncTable[291] := Func291;
end; { InitIdent }


function TSynCPMSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }


function TSynCPMSyn.KeyComp(const aKey: String): Boolean;
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
end; { KeyComp }


function TSynCPMSyn.Func15: TtkTokenKind;
begin
  if KeyComp('IF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func21: TtkTokenKind;
begin
  if KeyComp('OF') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func23: TtkTokenKind;
begin
  if KeyComp('END') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func28: TtkTokenKind;
begin
  if KeyComp('CASE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func29: TtkTokenKind;
begin
  if KeyComp('CHR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func30: TtkTokenKind;
begin
  if KeyComp('DECR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func37: TtkTokenKind;
begin
  if KeyComp('FOLD') then Result := tkKey else
    if KeyComp('BEGIN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func41: TtkTokenKind;
begin
  if KeyComp('ELSE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func43: TtkTokenKind;
begin
  if KeyComp('BLOCK') then Result := tkKey else
    if KeyComp('LOCAL') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func44: TtkTokenKind;
begin
  if KeyComp('INCR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func47: TtkTokenKind;
begin
  if KeyComp('THEN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func48: TtkTokenKind;
begin
  if KeyComp('MERGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func49: TtkTokenKind;
begin
  if KeyComp('PARAM') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func53: TtkTokenKind;
begin
  if KeyComp('SUM') then Result := tkSystem else
    if KeyComp('DIVIDE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func54: TtkTokenKind;
begin
  if KeyComp('RAVAL') then Result := tkSystem else
    if KeyComp('SQR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func55: TtkTokenKind;
begin
  if KeyComp('FILEEND') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func56: TtkTokenKind;
begin
  if KeyComp('MEMBER') then Result := tkKey else
    if KeyComp('FLOW') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func57: TtkTokenKind;
begin
  if KeyComp('WHILE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func58: TtkTokenKind;
begin
  if KeyComp('LOOP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func62: TtkTokenKind;
begin
  if KeyComp('FILEDATE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func63: TtkTokenKind;
begin
  if KeyComp('CLIENT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func65: TtkTokenKind;
begin
  if KeyComp('REPEAT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func66: TtkTokenKind;
begin
  if KeyComp('LENGTH') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func68: TtkTokenKind;
begin
  if KeyComp('GLOBALS') then Result := tkKey else
    if KeyComp('INCLUDE') then Result := tkKey else
      if KeyComp('ROOT') then Result := tkSystem else
        if KeyComp('LANGUAGE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func69: TtkTokenKind;
begin
  if KeyComp('ASSIGN') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func72: TtkTokenKind;
begin
  if KeyComp('ROUND') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func74: TtkTokenKind;
begin
  if KeyComp('FOREIGN') then Result := tkSystem else
    if KeyComp('PARENT') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func75: TtkTokenKind;
begin
  if KeyComp('MATCHING') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func76: TtkTokenKind;
begin
  if KeyComp('USEDBY') then Result := tkKey else
    if KeyComp('TRUNC') then Result := tkSystem else
      if KeyComp('RASTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func79: TtkTokenKind;
begin
  if KeyComp('FILETIME') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func83: TtkTokenKind;
begin
  if KeyComp('FILEDELETE') then Result := tkSystem else
    if KeyComp('V_DATE') then Result := tkSpecialVar else
      if KeyComp('MIDSTR') then Result := tkSystem else
        if KeyComp('EXECUTE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func85: TtkTokenKind;
begin
  if KeyComp('PARSEINC') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func86: TtkTokenKind;
begin
  if KeyComp('FILEREADLN') then Result := tkSystem else
    if KeyComp('FILECLOSE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func88: TtkTokenKind;
begin
  if KeyComp('FILEAPPEND') then Result := tkSystem else
    if KeyComp('SQL_ADD') then Result := tkSQLKey else
      if KeyComp('LTRUNC') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func89: TtkTokenKind;
begin
  if KeyComp('VARIABLES') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCPMSyn.Func91: TtkTokenKind;
begin
  if KeyComp('FILESIZE') then Result := tkSystem else
    if KeyComp('COPYFILE') then Result := tkSystem else
      if KeyComp('FILECOPY') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func92: TtkTokenKind;
begin
  if KeyComp('PDRIVER') then Result := tkKey else
    if KeyComp('MESSAGEDLG') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func94: TtkTokenKind;
begin
  if KeyComp('CATEGORY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func95: TtkTokenKind;
begin
  if KeyComp('METAFLOW') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func96: TtkTokenKind;
begin
  if KeyComp('V_FALSE') then Result := tkSpecialVar else Result := tkIdentifier;
end;

function TSynCPMSyn.Func99: TtkTokenKind;
begin
  if KeyComp('FILERESET') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func100: TtkTokenKind;
begin
  if KeyComp('LLENSTR') then Result := tkSystem else
    if KeyComp('LEFTSTR') then Result := tkSystem else
      if KeyComp('V_TIME') then Result := tkSpecialVar else
        if KeyComp('PRINTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func101: TtkTokenKind;
begin
  if KeyComp('FILEASSIGN') then Result := tkSystem else
    if KeyComp('CONTINUE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func104: TtkTokenKind;
begin
  if KeyComp('SUBTRACT') then Result := tkSystem else
    if KeyComp('FILESORT') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func105: TtkTokenKind;
begin
  if KeyComp('DEFINITION') then Result := tkKey else
    if KeyComp('SQL_EOF') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func106: TtkTokenKind;
begin
  if KeyComp('RLENSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func107: TtkTokenKind;
begin
  if KeyComp('STRPOS') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func108: TtkTokenKind;
begin
  if KeyComp('OPTIONS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func109: TtkTokenKind;
begin
  if KeyComp('SORTUP') then Result := tkKey else
    if KeyComp('PRINTFILE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func112: TtkTokenKind;
begin
  if KeyComp('LOCASESTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func113: TtkTokenKind;
begin
  if KeyComp('OUTPUT') then Result := tkKey else
    if KeyComp('SORTKEY') then Result := tkKey else
      if KeyComp('READINSTANCE') then Result := tkSystem else
        if KeyComp('SQL_MLADD') then Result := tkSQLKey else
          if KeyComp('SQL_FREE') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func114: TtkTokenKind;
begin
  if KeyComp('GROUPUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func116: TtkTokenKind;
begin
  if KeyComp('SUPPLIER') then Result := tkKey else
    if KeyComp('COUNTRY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func117: TtkTokenKind;
begin
  if KeyComp('V_TRUE') then Result := tkSpecialVar else
    if KeyComp('EQUALVALUE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func118: TtkTokenKind;
begin
  if KeyComp('GROUPHEADER') then Result := tkKey else
    if KeyComp('GROUPKEY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func119: TtkTokenKind;
begin
  if KeyComp('RIGHTSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func120: TtkTokenKind;
begin
  if KeyComp('ENTITYCODE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func122: TtkTokenKind;
begin
  if KeyComp('CENTERSTR') then Result := tkSystem else
    if KeyComp('UPCASESTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func125: TtkTokenKind;
begin
  if KeyComp('CONSTANTS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func126: TtkTokenKind;
begin
  if KeyComp('ALLENTITIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func127: TtkTokenKind;
begin
  if KeyComp('FILTERSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func128: TtkTokenKind;
begin
  if KeyComp('FILEEXISTS') then Result := tkSystem else
    if KeyComp('MULTIPLY') then Result := tkSystem else
      if KeyComp('SORTDOWN') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func130: TtkTokenKind;
begin
  if KeyComp('FILEREWRITE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func131: TtkTokenKind;
begin
  if KeyComp('SQL_CREATE') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func133: TtkTokenKind;
begin
  if KeyComp('FILEWRITELN') then Result := tkSystem else
    if KeyComp('GROUPDOWN') then Result := tkKey else
      if KeyComp('SQL_DUMP') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func134: TtkTokenKind;
begin
  if KeyComp('SHOWMESSAGE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func136: TtkTokenKind;
begin
  if KeyComp('EMPTYSHEET') then Result := tkKey else
    if KeyComp('CHARRLENSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func137: TtkTokenKind;
begin
  if KeyComp('V_NONEREAL') then Result := tkSpecialVar else
    if KeyComp('LASTINSTANCE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func138: TtkTokenKind;
begin
  if KeyComp('GLOBALVARIABLES') then Result := tkKey else
    if KeyComp('REPEATCOUNT') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func139: TtkTokenKind;
begin
  if KeyComp('STRIPSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func141: TtkTokenKind;
begin
  if KeyComp('ALLPRODUCTS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func142: TtkTokenKind;
begin
  if KeyComp('ORGANISATION') then Result := tkKey else
    if KeyComp('SQL_NEXT') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func143: TtkTokenKind;
begin
  if KeyComp('EQUALSTRING') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func146: TtkTokenKind;
begin
  if KeyComp('PREVINSTANCE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func147: TtkTokenKind;
begin
  if KeyComp('CHARREPLACESTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func148: TtkTokenKind;
begin
  if KeyComp('NEXTINSTANCE') then Result := tkSystem else
    if KeyComp('REPORTLEVEL') then Result := tkKey else
      if KeyComp('CHARRLLENSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func149: TtkTokenKind;
begin
  if KeyComp('VAROPTIONS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func153: TtkTokenKind;
begin
  if KeyComp('SQL_ASFLOAT') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func154: TtkTokenKind;
begin
  if KeyComp('SKIPEMTPTY') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func156: TtkTokenKind;
begin
  if KeyComp('GROUPFOOTER') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func157: TtkTokenKind;
begin
  if KeyComp('FIRSTINSTANCE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func160: TtkTokenKind;
begin
  if KeyComp('ALLSUPPLIERS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func162: TtkTokenKind;
begin
  if KeyComp('SQL_EXECUTE') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func164: TtkTokenKind;
begin
  if KeyComp('SQL_SETVAR') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func165: TtkTokenKind;
begin
  if KeyComp('STROPTIONS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func166: TtkTokenKind;
begin
  if KeyComp('ALLPROPERTIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func170: TtkTokenKind;
begin
  if KeyComp('ZERORLENSTR') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func174: TtkTokenKind;
begin
  if KeyComp('GLOBALCONSTANTS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func178: TtkTokenKind;
begin
  if KeyComp('READREPEATINSTANCE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func186: TtkTokenKind;
begin
  if KeyComp('SQL_ASSTRING') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func187: TtkTokenKind;
begin
  if KeyComp('V_PAR_LANGUAGE') then Result := tkSpecialVar else Result := tkIdentifier;
end;

function TSynCPMSyn.Func188: TtkTokenKind;
begin
  if KeyComp('SQL_MLMULTIADD') then Result := tkSQLKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func198: TtkTokenKind;
begin
  if KeyComp('LOWERLEVELSTOO') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func210: TtkTokenKind;
begin
  if KeyComp('PROPERTYGROUP') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func211: TtkTokenKind;
begin
  if KeyComp('PREVREPEATINSTANCE') then Result := tkSystem else Result := tkIdentifier;
end;

function TSynCPMSyn.Func212: TtkTokenKind;
begin
  if KeyComp('DISTINCT_EXECUTE') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func213: TtkTokenKind;
begin
  if KeyComp('NEXTREPEATINSTANCE') then Result := tkSystem else
    if KeyComp('SUPPLIESOFMEMBERS') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func271: TtkTokenKind;
begin
  if KeyComp('ALLQUALITYPROPERTIES') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCPMSyn.Func273: TtkTokenKind;
begin
  if KeyComp('V_PAR_LANGUAGE_FIELDS') then Result := tkSpecialVar else Result := tkIdentifier;
end;

function TSynCPMSyn.Func291: TtkTokenKind;
begin
  if KeyComp('V_PAR_LANGUAGE_COUNT') then Result := tkSpecialVar else Result := tkIdentifier;
end;

function TSynCPMSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end; { AltFunc }


function TSynCPMSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end; { IdentKind }


procedure TSynCPMSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      #0       : fProcTable[I] := NullProc;
      #10      : fProcTable[I] := LFProc;
      #13      : fProcTable[I] := CRProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      '"'      : fProcTable[I] := StringProc;
      '0'..'9' : fProcTable[I] := NumberProc;
      'A'..'Z',
      'a'..'z',
      '_'      : case I of
                   'V', 'v',
                   'S', 's'  : fProcTable[I] := VariableProc;
                 else
                   fProcTable[I] := IdentProc;
                 end;
      '{'      : fProcTable[I] := BraceOpenProc;
      '}',
      '!',
      '%',
      '&',
      '('..'/',
      ':'..'@',
      '['..'^',
      '`', '~' : begin
                   case I of
                     ';': fProcTable[I] := SemiColonProc;
                   else
                     fProcTable[I] := SymbolProc;
                   end;
                 end;
    else
      fProcTable[I] := UnknownProc;
    end;
  end;
end; { MakeMethodTables }


constructor TSynCPMSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clNavy;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clGreen;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  
  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSQLKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrSQLKey);
  fSQLKeyAttri.ForeGround := clTeal;
  fSQLKeyAttri.Style := [fsBold];
  AddAttribute(fSQLKeyAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  fSpecialVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable);
  fSpecialVarAttri.Style := [fsBold];
  AddAttribute(fSpecialVarAttri);

  fSystemAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem);
  fSystemAttri.Foreground := $000080FF;
  fSystemAttri.Style := [fsBold];
  AddAttribute(fSystemAttri);

  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  fVariableAttri.Foreground := clMaroon;
  AddAttribute(fVariableAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fCommentLevel := 0;
  fDefaultFilter := SYNS_FilterCPM;
end; { Create }


procedure TSynCPMSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }


procedure TSynCPMSyn.BraceOpenProc;
begin
  fRange := rsBraceComment;
  BraceCommentProc;
  fTokenID := tkComment;
end; { BraceOpenProc }


procedure TSynCPMSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end; { IdentProc }


procedure TSynCPMSyn.VariableProc;
begin
  fTokenID := IdentKind((fLine + Run));
  if (fTokenID = tkIdentifier) then
  begin
    if (fLine[Run + 1] = '_') then
      fTokenID := tkVariable
  end;
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end; { VariableProc }


procedure TSynCPMSyn.NullProc;
begin
  fTokenID := tkNull;
end; { NullProc }


procedure TSynCPMSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end; { SpaceProc }


procedure TSynCPMSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13, '"'];
  if (fLine[Run] = '"') then
  begin
    Inc(Run);
    if (fLine[Run] = '"') then
      Inc(Run);
  end;
end; { StringProc }


procedure TSynCPMSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end; { UnknownProc }


procedure TSynCPMSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsBraceComment: BraceCommentProc;
  else
    fProcTable[fLine[Run]];
  end;
end; { Next }


function TSynCPMSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD   : Result := fKeyAttri;
    SYN_ATTR_STRING    : Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL    : Result := fSymbolAttri;
    else
      Result := nil;
  end;
end; { GetDefaultAttribute }


function TSynCPMSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end; { GetEol }


function TSynCPMSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end; { GetToken }


function TSynCPMSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end; { GetTokenID }


function TSynCPMSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkSQLKey: Result := fSQLKeyAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSpecialVar: Result := fSpecialVarAttri;
    tkSystem: Result := fSystemAttri;
    tkVariable: Result := fVariableAttri; 
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end; { GetTokenAttribute }


function TSynCPMSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end; { GetTokenKind }


function TSynCPMSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end; { GetTokenPos }


function TSynCPMSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end; { getIdentChars }


class function TSynCPMSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCPM;
end; { GetLanguageName }


procedure TSynCPMSyn.BraceCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if fLine[Run] = '{' then
          Inc(fCommentLevel)
        else if fLine[Run] = '}' then
        begin
          Dec(fCommentLevel);
          if (fCommentLevel < 1) then
          begin
            Inc(Run);
            fRange := rsUnKnown;
            fCommentLevel := 0;
            Break;
          end;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end; { BraceCommentProc }


procedure TSynCPMSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end; { CRProc }


procedure TSynCPMSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end; { LFProc }


function TSynCPMSyn.GetSampleSource: string;
begin
  Result := '{ COAS Product Manager report (RDF) }'#13#10 +
            'PARAM'#13#10 +
            '  LANGUAGE;'#13#10 +
            '  CONTINUE;'#13#10 +
            'END; { Param }'#13#10 +
            #13#10 +
            'GLOBALS'#13#10 +
            '  LANGUAGE = LOCAL;'#13#10 +
            'END; { Globals }'#13#10 +
            #13#10 +
            'DEFINITION BLOCK "MAIN"'#13#10 +
            'VARIABLES'#13#10 +
            '  S_Query = "";'#13#10 +
            '  V_OraErr = -1;'#13#10 +
            '  V_Count;'#13#10 +
            'BEGIN'#13#10 +
            '  ASSIGN(S_Query, "SELECT * FROM DUAL");'#13#10 +
            '  SQL_CREATE(V_OraErr, S_Query);'#13#10 +
            '  ASSIGN(V_Count, V_NoneReal);'#13#10 +
            'END;';
end; { GetSampleSource }


function TSynCPMSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPM;
end; { IsFilterStored }


procedure TSynCPMSyn.SemiColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end; { SemiColonProc }


procedure TSynCPMSyn.NumberProc;
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


procedure TSynCPMSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end; { SymbolProc }


procedure TSynCPMSyn.ResetRange;
begin
  inherited;
  fRange := rsUnknown;
  fCommentLevel := 0;
end; { ResetRange }


procedure TSynCPMSyn.SetRange(Value: Pointer);
var
  AValue: LongInt;
begin
  inherited;
  AValue := Longint(Value);
  fCommentLevel := AValue div $10000;
  fRange := TRangeState(AValue mod $10000);
end; { SetRange }


function TSynCPMSyn.GetRange: Pointer;
begin
  Result := Pointer((fCommentLevel * $10000) + Integer(fRange));
end; { GetRange }


initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            
  RegisterPlaceableHighlighter(TSynCPMSyn);
{$ENDIF}
end.
