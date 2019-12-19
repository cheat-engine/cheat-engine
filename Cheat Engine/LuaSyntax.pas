{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: Lua.pas, released 2004-10-27.
Description: Lua Syntax Parser/Highlighter
The initial author of this file is Jean-Franois Goulet.
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

$Id: LuaSyntax.pas,v 1.1 2006/11/21 00:42:58 jfgoulet Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit LuaSyntax;


{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

{$DEFINE SYNEDIT_INCLUDE}

{$IFdef MSWindows}
  {$DEFINE SYN_WIN32}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SYN_COMPILER_5}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_5}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SYN_COMPILER_4}
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SYN_COMPILER_4}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SYN_COMPILER_3}
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE SYN_COMPILER_3}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_3}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE SYN_COMPILER_2}  { C++B v1 compiler is really v2 }
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_1}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE SYN_COMPILER_2}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2}
{$ENDIF}

{$IFDEF SYN_COMPILER_2}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_3}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_4}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_5}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2}
  {$DEFINE SYN_DELPHI_2_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_3}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_4}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_5}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
{$ENDIF}

{$IFDEF SYN_CPPB_3}
  {$DEFINE SYN_CPPB_3_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
  {$DEFINE SYN_NO_COM_CLEANUP}
{$ENDIF}

{$IFDEF SYN_CPPB_3_UP}
  // C++Builder requires this if you use Delphi components in run-time packages.
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_LAZARUS}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}

// defaults are short evaluation of boolean values and long strings

// lazarus change   no $B-
{$H+}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}

// support for multibyte character sets
{$IFDEF SYN_COMPILER_3_UP}
{$IFNDEF SYN_LAZARUS}
{$DEFINE SYN_MBCSSUPPORT}
{$ENDIF}
{$ENDIF}

// additional tests for debugging

{.$DEFINE SYN_DEVELOPMENT_CHECKS}

{$IFDEF SYN_DEVELOPMENT_CHECKS}

{$R+,Q+,S+,T+}

{$ENDIF}

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
  Classes,
  StringHashList,
  SynEditHighlighterFoldBase,
  LCLType,
  Registry;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkKeySecondary,
    tkKeyTertiary,
    tkKeyQuaternary,
    tkLuaMString,
    tkNull,
    tkNumber,
    tkOctal,
    tkHex,
    tkFloat,
    tkSpace,
    tkString,
    tkInternalFunction,
    tkUnknown);

  TRangeState = (rsUnKnown, rsLuaComment, rsLuaMComment, rsLuaMString, rsString1, rsString2);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 236;

type
  //TSynLuaSyn = class(TSynCustomHighlighter)
  TSynLuaSyn = class(TSynCustomFoldHighlighter)
  private
    FTokenPos, FTokenEnd: Integer;
    FLineText: String;

    StartCodeFold: boolean;
    EndCodeFold: boolean;

    //FCurRange: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRangeExtended: ptrUInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fKeySecondaryAttri: TSynHighlighterAttributes;
    fKeyTertiaryAttri: TSynHighlighterAttributes;
    fKeyQuaternaryAttri: TSynHighlighterAttributes;
    fLuaMStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fInternalFunctionAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func7: TtkTokenKind;
    function Func12: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func147: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func165: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func175: TtkTokenKind;
    function Func180: TtkTokenKind;
    function Func185: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func190: TtkTokenKind;
    function Func192: TtkTokenKind;
    function Func193: TtkTokenKind;
    function Func197: TtkTokenKind;
    function Func202: TtkTokenKind;
    function Func209: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func217: TtkTokenKind;
    function Func225: TtkTokenKind;
    function Func234: TtkTokenKind;
    function Func236: TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure LuaCommentOpenProc;
    procedure LuaCommentProc;
    procedure LuaMCommentOpenProc;
    procedure LuaMCommentProc;
    procedure LuaMStringOpenProc;
    procedure LuaMStringProc;
    procedure String1OpenProc;
    procedure String1Proc;
    procedure String2OpenProc;
    procedure String2Proc;
    procedure NumberProc;
    function LongDelimCheck(aRun: integer): integer;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;

    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;


    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property KeySecondaryAttri: TSynHighlighterAttributes read fKeySecondaryAttri write fKeySecondaryAttri;
    property KeyTertiaryAttri: TSynHighlighterAttributes read fKeyTertiaryAttri write fKeyTertiaryAttri;
    property KeyQuaternaryAttri: TSynHighlighterAttributes read fKeyQuaternaryAttri write fKeyQuaternaryAttri;
    property InternalFunctionAttri: TSynHighlighterAttributes read fInternalFunctionAttri write fInternalFunctionAttri;
    property LuaMStringAttri: TSynHighlighterAttributes read fLuaMStringAttri write fLuaMStringAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri write fHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;

var luasyntaxStringHashList: TStringHashList;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst, math;
{$ELSE}
  SynEditStrConst, math;
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SYNS_FilterLua = 'Lua Files (*.lua, *.lpr)|*.lua;*.lpr';
  SYNS_LangLua = 'Lua';
  SYNS_AttrLuaMString = 'Multiline String';
  SYNS_AttrReservedWord2 = 'Reserved word 2';
  SYNS_AttrReservedWord3 = 'Reserved word 3';
  SYNS_AttrReservedWord4 = 'Reserved word 4';
  SYNS_AttrNumber = 'Numbers';

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;




procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['.', '_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            mHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mHashTable[I] := Ord(I) - 95
          else if (I = #46) then
            mHashTable[I] := 1;
        end;
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynLuaSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := {$IFDEF FPC}@{$ENDIF}AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[7] := {$IFDEF FPC}@{$ENDIF}Func7;
  fIdentFuncTable[12] := {$IFDEF FPC}@{$ENDIF}Func12;
  fIdentFuncTable[17] := {$IFDEF FPC}@{$ENDIF}Func17;
  fIdentFuncTable[19] := {$IFDEF FPC}@{$ENDIF}Func19;
  fIdentFuncTable[21] := {$IFDEF FPC}@{$ENDIF}Func21;
  fIdentFuncTable[22] := {$IFDEF FPC}@{$ENDIF}Func22;
  fIdentFuncTable[24] := {$IFDEF FPC}@{$ENDIF}Func24;
  fIdentFuncTable[25] := {$IFDEF FPC}@{$ENDIF}Func25;
  fIdentFuncTable[26] := {$IFDEF FPC}@{$ENDIF}Func26;
  fIdentFuncTable[32] := {$IFDEF FPC}@{$ENDIF}Func32;
  fIdentFuncTable[34] := {$IFDEF FPC}@{$ENDIF}Func34;
  fIdentFuncTable[35] := {$IFDEF FPC}@{$ENDIF}Func35;
  fIdentFuncTable[36] := {$IFDEF FPC}@{$ENDIF}Func36;
  fIdentFuncTable[38] := {$IFDEF FPC}@{$ENDIF}Func38;
  fIdentFuncTable[41] := {$IFDEF FPC}@{$ENDIF}Func41;
  fIdentFuncTable[42] := {$IFDEF FPC}@{$ENDIF}Func42;
  fIdentFuncTable[44] := {$IFDEF FPC}@{$ENDIF}Func44;
  fIdentFuncTable[45] := {$IFDEF FPC}@{$ENDIF}Func45;
  fIdentFuncTable[46] := {$IFDEF FPC}@{$ENDIF}Func46;
  fIdentFuncTable[48] := {$IFDEF FPC}@{$ENDIF}Func48;
  fIdentFuncTable[49] := {$IFDEF FPC}@{$ENDIF}Func49;
  fIdentFuncTable[51] := {$IFDEF FPC}@{$ENDIF}Func51;
  fIdentFuncTable[52] := {$IFDEF FPC}@{$ENDIF}Func52;
  fIdentFuncTable[57] := {$IFDEF FPC}@{$ENDIF}Func57;
  fIdentFuncTable[59] := {$IFDEF FPC}@{$ENDIF}Func59;
  fIdentFuncTable[61] := {$IFDEF FPC}@{$ENDIF}Func61;
  fIdentFuncTable[62] := {$IFDEF FPC}@{$ENDIF}Func62;
  fIdentFuncTable[66] := {$IFDEF FPC}@{$ENDIF}Func66;
  fIdentFuncTable[67] := {$IFDEF FPC}@{$ENDIF}Func67;
  fIdentFuncTable[68] := {$IFDEF FPC}@{$ENDIF}Func68;
  fIdentFuncTable[70] := {$IFDEF FPC}@{$ENDIF}Func70;
  fIdentFuncTable[71] := {$IFDEF FPC}@{$ENDIF}Func71;
  fIdentFuncTable[72] := {$IFDEF FPC}@{$ENDIF}Func72;
  fIdentFuncTable[73] := {$IFDEF FPC}@{$ENDIF}Func73;
  fIdentFuncTable[74] := {$IFDEF FPC}@{$ENDIF}Func74;
  fIdentFuncTable[76] := {$IFDEF FPC}@{$ENDIF}Func76;
  fIdentFuncTable[78] := {$IFDEF FPC}@{$ENDIF}Func78;
  fIdentFuncTable[79] := {$IFDEF FPC}@{$ENDIF}Func79;
  fIdentFuncTable[80] := {$IFDEF FPC}@{$ENDIF}Func80;
  fIdentFuncTable[81] := {$IFDEF FPC}@{$ENDIF}Func81;
  fIdentFuncTable[84] := {$IFDEF FPC}@{$ENDIF}Func84;
  fIdentFuncTable[85] := {$IFDEF FPC}@{$ENDIF}Func85;
  fIdentFuncTable[86] := {$IFDEF FPC}@{$ENDIF}Func86;
  fIdentFuncTable[87] := {$IFDEF FPC}@{$ENDIF}Func87;
  fIdentFuncTable[88] := {$IFDEF FPC}@{$ENDIF}Func88;
  fIdentFuncTable[89] := {$IFDEF FPC}@{$ENDIF}Func89;
  fIdentFuncTable[91] := {$IFDEF FPC}@{$ENDIF}Func91;
  fIdentFuncTable[92] := {$IFDEF FPC}@{$ENDIF}Func92;
  fIdentFuncTable[93] := {$IFDEF FPC}@{$ENDIF}Func93;
  fIdentFuncTable[94] := {$IFDEF FPC}@{$ENDIF}Func94;
  fIdentFuncTable[95] := {$IFDEF FPC}@{$ENDIF}Func95;
  fIdentFuncTable[96] := {$IFDEF FPC}@{$ENDIF}Func96;
  fIdentFuncTable[97] := {$IFDEF FPC}@{$ENDIF}Func97;
  fIdentFuncTable[98] := {$IFDEF FPC}@{$ENDIF}Func98;
  fIdentFuncTable[99] := {$IFDEF FPC}@{$ENDIF}Func99;
  fIdentFuncTable[100] := {$IFDEF FPC}@{$ENDIF}Func100;
  fIdentFuncTable[101] := {$IFDEF FPC}@{$ENDIF}Func101;
  fIdentFuncTable[102] := {$IFDEF FPC}@{$ENDIF}Func102;
  fIdentFuncTable[104] := {$IFDEF FPC}@{$ENDIF}Func104;
  fIdentFuncTable[105] := {$IFDEF FPC}@{$ENDIF}Func105;
  fIdentFuncTable[106] := {$IFDEF FPC}@{$ENDIF}Func106;
  fIdentFuncTable[107] := {$IFDEF FPC}@{$ENDIF}Func107;
  fIdentFuncTable[108] := {$IFDEF FPC}@{$ENDIF}Func108;
  fIdentFuncTable[110] := {$IFDEF FPC}@{$ENDIF}Func110;
  fIdentFuncTable[112] := {$IFDEF FPC}@{$ENDIF}Func112;
  fIdentFuncTable[113] := {$IFDEF FPC}@{$ENDIF}Func113;
  fIdentFuncTable[114] := {$IFDEF FPC}@{$ENDIF}Func114;
  fIdentFuncTable[115] := {$IFDEF FPC}@{$ENDIF}Func115;
  fIdentFuncTable[116] := {$IFDEF FPC}@{$ENDIF}Func116;
  fIdentFuncTable[117] := {$IFDEF FPC}@{$ENDIF}Func117;
  fIdentFuncTable[118] := {$IFDEF FPC}@{$ENDIF}Func118;
  fIdentFuncTable[121] := {$IFDEF FPC}@{$ENDIF}Func121;
  fIdentFuncTable[122] := {$IFDEF FPC}@{$ENDIF}Func122;
  fIdentFuncTable[123] := {$IFDEF FPC}@{$ENDIF}Func123;
  fIdentFuncTable[125] := {$IFDEF FPC}@{$ENDIF}Func125;
  fIdentFuncTable[126] := {$IFDEF FPC}@{$ENDIF}Func126;
  fIdentFuncTable[127] := {$IFDEF FPC}@{$ENDIF}Func127;
  fIdentFuncTable[128] := {$IFDEF FPC}@{$ENDIF}Func128;
  fIdentFuncTable[129] := {$IFDEF FPC}@{$ENDIF}Func129;
  fIdentFuncTable[130] := {$IFDEF FPC}@{$ENDIF}Func130;
  fIdentFuncTable[131] := {$IFDEF FPC}@{$ENDIF}Func131;
  fIdentFuncTable[132] := {$IFDEF FPC}@{$ENDIF}Func132;
  fIdentFuncTable[133] := {$IFDEF FPC}@{$ENDIF}Func133;
  fIdentFuncTable[135] := {$IFDEF FPC}@{$ENDIF}Func135;
  fIdentFuncTable[136] := {$IFDEF FPC}@{$ENDIF}Func136;
  fIdentFuncTable[137] := {$IFDEF FPC}@{$ENDIF}Func137;
  fIdentFuncTable[138] := {$IFDEF FPC}@{$ENDIF}Func138;
  fIdentFuncTable[139] := {$IFDEF FPC}@{$ENDIF}Func139;
  fIdentFuncTable[140] := {$IFDEF FPC}@{$ENDIF}Func140;
  fIdentFuncTable[143] := {$IFDEF FPC}@{$ENDIF}Func143;
  fIdentFuncTable[144] := {$IFDEF FPC}@{$ENDIF}Func144;
  fIdentFuncTable[145] := {$IFDEF FPC}@{$ENDIF}Func145;
  fIdentFuncTable[146] := {$IFDEF FPC}@{$ENDIF}Func146;
  fIdentFuncTable[147] := {$IFDEF FPC}@{$ENDIF}Func147;
  fIdentFuncTable[150] := {$IFDEF FPC}@{$ENDIF}Func150;
  fIdentFuncTable[152] := {$IFDEF FPC}@{$ENDIF}Func152;
  fIdentFuncTable[155] := {$IFDEF FPC}@{$ENDIF}Func155;
  fIdentFuncTable[165] := {$IFDEF FPC}@{$ENDIF}Func165;
  fIdentFuncTable[168] := {$IFDEF FPC}@{$ENDIF}Func168;
  fIdentFuncTable[172] := {$IFDEF FPC}@{$ENDIF}Func172;
  fIdentFuncTable[173] := {$IFDEF FPC}@{$ENDIF}Func173;
  fIdentFuncTable[175] := {$IFDEF FPC}@{$ENDIF}Func175;
  fIdentFuncTable[180] := {$IFDEF FPC}@{$ENDIF}Func180;
  fIdentFuncTable[185] := {$IFDEF FPC}@{$ENDIF}Func185;
  fIdentFuncTable[188] := {$IFDEF FPC}@{$ENDIF}Func188;
  fIdentFuncTable[190] := {$IFDEF FPC}@{$ENDIF}Func190;
  fIdentFuncTable[192] := {$IFDEF FPC}@{$ENDIF}Func192;
  fIdentFuncTable[193] := {$IFDEF FPC}@{$ENDIF}Func193;
  fIdentFuncTable[197] := {$IFDEF FPC}@{$ENDIF}Func197;
  fIdentFuncTable[202] := {$IFDEF FPC}@{$ENDIF}Func202;
  fIdentFuncTable[209] := {$IFDEF FPC}@{$ENDIF}Func209;
  fIdentFuncTable[213] := {$IFDEF FPC}@{$ENDIF}Func213;
  fIdentFuncTable[217] := {$IFDEF FPC}@{$ENDIF}Func217;
  fIdentFuncTable[225] := {$IFDEF FPC}@{$ENDIF}Func225;
  fIdentFuncTable[234] := {$IFDEF FPC}@{$ENDIF}Func234;
  fIdentFuncTable[236] := {$IFDEF FPC}@{$ENDIF}Func236;
end;

function TSynLuaSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['.', '_', 'a'..'z', 'A'..'Z', '0'..'9'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynLuaSyn.KeyComp(const aKey: String): Boolean;
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
end;

function TSynLuaSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then
  begin
    StartCodeFold:=true;
    Result := tkKey;
  end
  else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func21: TtkTokenKind;
begin
  if KeyComp('do') then
  begin
    StartCodeFold:=true;
    Result := tkKey;
  end
  else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func22: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func25: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func26: TtkTokenKind;
begin
  if KeyComp('end') then
  begin
    EndCodeFold:=true;
    Result := tkKey;
  end
  else
  if KeyComp('io') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func35: TtkTokenKind;
begin
  if KeyComp('or') then Result := tkKey else
  if KeyComp('__mod') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func38: TtkTokenKind;
begin
  if KeyComp('nil') then Result := tkKey else
  if KeyComp('__div') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
  if KeyComp('break') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
  if KeyComp('table') then Result := tkKeySecondary else
  if KeyComp('__sub') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func48: TtkTokenKind;
begin
  if KeyComp('local') then Result := tkKey else
  if KeyComp('false') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func51: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey else
  if KeyComp('__unm') then Result := tkKeySecondary else
  if KeyComp('package') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func52: TtkTokenKind;
begin
  if KeyComp('not') then Result := tkKey else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func61: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else
  if KeyComp('__index') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func62: TtkTokenKind;
begin
  if KeyComp('elseif') then Result := tkKey else
  if KeyComp('while') then Result := tkKey else
  if KeyComp('__concat') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func68: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else
  if KeyComp('pairs') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func71: TtkTokenKind;
begin
  if KeyComp('repeat') then
  begin
    Result := tkKey;
    StartCodeFold:=true;
  end
  else
  if KeyComp('os.date') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func81: TtkTokenKind;
begin
  if KeyComp('until') then
  begin
    Result := tkKey;
    EndCodeFold:=true;
  end
  else
  if KeyComp('table.pack') then Result := tkKeyTertiary else
  if KeyComp('io.open') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func102: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else
  if KeyComp('_VERSION') then Result := tkKeySecondary else
  if KeyComp('table.maxn') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func110: TtkTokenKind;
begin
  if KeyComp('function') then
  begin
    Result := tkKey;
    StartCodeFold:=true;

  end
  else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func7: TtkTokenKind;
begin
  if KeyComp('_G') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func12: TtkTokenKind;
begin
  if KeyComp('__gc') then Result := tkKeySecondary else
  if KeyComp('__add') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func19: TtkTokenKind;
begin
  if KeyComp('__le') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func24: TtkTokenKind;
begin
  if KeyComp('__eq') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func32: TtkTokenKind;
begin
  if KeyComp('__call') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func34: TtkTokenKind;
begin
  if KeyComp('bit32') then Result := tkKeySecondary else
  if KeyComp('__lt') then Result := tkKeySecondary else
  if KeyComp('__len') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func36: TtkTokenKind;
begin
  if KeyComp('load') then Result := tkKeySecondary else
  if KeyComp('os') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func41: TtkTokenKind;
begin
  if KeyComp('_ENV') then Result := tkKeySecondary else
  if KeyComp('__mode') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func44: TtkTokenKind;
begin
  if KeyComp('debug') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func46: TtkTokenKind;
begin
  if KeyComp('math') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func49: TtkTokenKind;
begin
  if KeyComp('pcall') then Result := tkKeySecondary else
  if KeyComp('__mul') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func57: TtkTokenKind;
begin
  if KeyComp('dofile') then Result := tkKeySecondary else
  if KeyComp('__pow') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func59: TtkTokenKind;
begin
  if KeyComp('io.read') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func66: TtkTokenKind;
begin
  if KeyComp('math.deg') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func67: TtkTokenKind;
begin
  if KeyComp('next') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func70: TtkTokenKind;
begin
  if KeyComp('type') then Result := tkKeySecondary else
  if KeyComp('select') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func72: TtkTokenKind;
begin
  if KeyComp('loadfile') then Result := tkKeySecondary else
  if KeyComp('unpack') then Result := tkKeySecondary else
  if KeyComp('math.abs') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func73: TtkTokenKind;
begin
  if KeyComp('math.rad') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func74: TtkTokenKind;
begin
  if KeyComp('xpcall') then Result := tkKeySecondary else
  if KeyComp('math.pi') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func76: TtkTokenKind;
begin
  if KeyComp('module') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func78: TtkTokenKind;
begin
  if KeyComp('ipairs') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func79: TtkTokenKind;
begin
  if KeyComp('error') then Result := tkKeySecondary else
  if KeyComp('rawlen') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func80: TtkTokenKind;
begin
  if KeyComp('rawget') then Result := tkKeySecondary else
  if KeyComp('math.ceil') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func84: TtkTokenKind;
begin
  if KeyComp('math.log10') then Result := tkKeyTertiary else
  if KeyComp('math.log') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func85: TtkTokenKind;
begin
  if KeyComp('math.tan') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func86: TtkTokenKind;
begin
  if KeyComp('getfenv') then Result := tkKeySecondary else
  if KeyComp('math.min') then Result := tkKeyTertiary else
  if KeyComp('os.clock') then Result := tkKeyQuaternary else
  if KeyComp('io.close') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func87: TtkTokenKind;
begin
  if KeyComp('math.atan') then Result := tkKeyTertiary else
  if KeyComp('math.atan2') then Result := tkKeyTertiary else
  if KeyComp('math.cos') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func88: TtkTokenKind;
begin
  if KeyComp('assert') then Result := tkKeySecondary else
  if KeyComp('math.max') then Result := tkKeyTertiary else
  if KeyComp('os.time') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func89: TtkTokenKind;
begin
  if KeyComp('math.modf') then Result := tkKeyTertiary else
  if KeyComp('math.acos') then Result := tkKeyTertiary else
  if KeyComp('math.fmod') then Result := tkKeyTertiary else
  if KeyComp('debug.debug') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func91: TtkTokenKind;
begin
  if KeyComp('io.lines') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func92: TtkTokenKind;
begin
  if KeyComp('rawset') then Result := tkKeySecondary else
  if KeyComp('math.huge') then Result := tkKeyTertiary else
  if KeyComp('math.sin') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func93: TtkTokenKind;
begin
  if KeyComp('string') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func94: TtkTokenKind;
begin
  if KeyComp('math.tanh') then Result := tkKeyTertiary else
  if KeyComp('math.asin') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func95: TtkTokenKind;
begin
  if KeyComp('math.exp') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func96: TtkTokenKind;
begin
  if KeyComp('math.cosh') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func97: TtkTokenKind;
begin
  if KeyComp('io.type') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func98: TtkTokenKind;
begin
  if KeyComp('setfenv') then Result := tkKeySecondary else
  if KeyComp('io.popen') then Result := tkKeyQuaternary else
  if KeyComp('io.flush') then Result := tkKeyQuaternary else
  if KeyComp('io.stdin') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func99: TtkTokenKind;
begin
  if KeyComp('os.exit') then Result := tkKeyQuaternary else
  if KeyComp('package.loaded') then Result := tkKeyQuaternary else
  if KeyComp('os.rename') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func100: TtkTokenKind;
begin
  if KeyComp('require') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func101: TtkTokenKind;
begin
  if KeyComp('math.sinh') then Result := tkKeyTertiary else
  if KeyComp('package.path') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func104: TtkTokenKind;
begin
  if KeyComp('math.pow') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func105: TtkTokenKind;
begin
  if KeyComp('package.cpath') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func106: TtkTokenKind;
begin
  if KeyComp('rawequal') then Result := tkKeySecondary else
  if KeyComp('__newindex') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func107: TtkTokenKind;
begin
  if KeyComp('io.write') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func108: TtkTokenKind;
begin
  if KeyComp('table.concat') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func112: TtkTokenKind;
begin
  if KeyComp('package.seeall') then Result := tkKeyQuaternary else
  if KeyComp('io.input') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func113: TtkTokenKind;
begin
  if KeyComp('math.ldexp') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func114: TtkTokenKind;
begin
  if KeyComp('package.loadlib') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func115: TtkTokenKind;
begin
  if KeyComp('io.tmpfile') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func116: TtkTokenKind;
begin
  if KeyComp('tonumber') then Result := tkKeySecondary else
  if KeyComp('os.getenv') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func117: TtkTokenKind;
begin
  if KeyComp('os.difftime') then Result := tkKeyQuaternary else
  if KeyComp('io.stderr') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func118: TtkTokenKind;
begin
  if KeyComp('table.unpack') then Result := tkKeyTertiary else
  if KeyComp('math.floor') then Result := tkKeyTertiary else
  if KeyComp('math.random') then Result := tkKeyTertiary else
  if KeyComp('debug.traceback') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func121: TtkTokenKind;
begin
  if KeyComp('math.frexp') then Result := tkKeyTertiary else
  if KeyComp('os.remove') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func122: TtkTokenKind;
begin
  if KeyComp('table.sort') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func123: TtkTokenKind;
begin
  if KeyComp('getmetatable') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func125: TtkTokenKind;
begin
  if KeyComp('collectgarbage') then Result := tkKeySecondary else
  if KeyComp('math.sqrt') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func126: TtkTokenKind;
begin
  if KeyComp('os.tmpname') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func127: TtkTokenKind;
begin
  if KeyComp('os.execute') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func128: TtkTokenKind;
begin
  if KeyComp('string.len') then Result := tkKeyTertiary else
  if KeyComp('string.char') then Result := tkKeyTertiary else
  if KeyComp('debug.getlocal') then Result := tkKeyQuaternary else
  if KeyComp('debug.getinfo') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func129: TtkTokenKind;
begin
  if KeyComp('loadstring') then Result := tkKeySecondary else
  if KeyComp('coroutine') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func130: TtkTokenKind;
begin
  if KeyComp('tostring') then Result := tkKeySecondary else
  if KeyComp('table.remove') then Result := tkKeyTertiary else
  if KeyComp('package.preload') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func131: TtkTokenKind;
begin
  if KeyComp('string.find') then Result := tkKeyTertiary else
  if KeyComp('debug.getfenv') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func132: TtkTokenKind;
begin
  if KeyComp('io.stdout') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func133: TtkTokenKind;
begin
  if KeyComp('debug.gethook') then Result := tkKeyQuaternary else
  if KeyComp('package.loaders') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func135: TtkTokenKind;
begin
  if KeyComp('setmetatable') then Result := tkKeySecondary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func136: TtkTokenKind;
begin
  if KeyComp('string.rep') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func137: TtkTokenKind;
begin
  if KeyComp('table.insert') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func138: TtkTokenKind;
begin
  if KeyComp('os.setlocale') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func139: TtkTokenKind;
begin
  if KeyComp('string.sub') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func140: TtkTokenKind;
begin
  if KeyComp('debug.setlocal') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func143: TtkTokenKind;
begin
  if KeyComp('debug.setfenv') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func144: TtkTokenKind;
begin
  if KeyComp('string.match') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func145: TtkTokenKind;
begin
  if KeyComp('debug.sethook') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func146: TtkTokenKind;
begin
  if KeyComp('io.output') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func147: TtkTokenKind;
begin
  if KeyComp('string.gsub') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func150: TtkTokenKind;
begin
  if KeyComp('string.byte') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func152: TtkTokenKind;
begin
  if KeyComp('string.dump') then Result := tkKeyTertiary else
  if KeyComp('string.gmatch') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func155: TtkTokenKind;
begin
  if KeyComp('math.randomseed') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func165: TtkTokenKind;
begin
  if KeyComp('debug.upvalueid') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func168: TtkTokenKind;
begin
  if KeyComp('debug.getmetatable') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func172: TtkTokenKind;
begin
  if KeyComp('string.lower') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func173: TtkTokenKind;
begin
  if KeyComp('string.format') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func175: TtkTokenKind;
begin
  if KeyComp('string.upper') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func180: TtkTokenKind;
begin
  if KeyComp('debug.setmetatable') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func185: TtkTokenKind;
begin
  if KeyComp('debug.getupvalue') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func188: TtkTokenKind;
begin
  if KeyComp('coroutine.create') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func190: TtkTokenKind;
begin
  if KeyComp('coroutine.yield') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func192: TtkTokenKind;
begin
  if KeyComp('coroutine.wrap') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func193: TtkTokenKind;
begin
  if KeyComp('string.reverse') then Result := tkKeyTertiary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func197: TtkTokenKind;
begin
  if KeyComp('debug.setupvalue') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func202: TtkTokenKind;
begin
  if KeyComp('debug.upvaluejoin') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func209: TtkTokenKind;
begin
  if KeyComp('debug.getregistry') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func213: TtkTokenKind;
begin
  if KeyComp('debug.getuservalue') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func217: TtkTokenKind;
begin
  if KeyComp('coroutine.resume') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func225: TtkTokenKind;
begin
  if KeyComp('debug.setuservalue') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func234: TtkTokenKind;
begin
  if KeyComp('coroutine.running') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.Func236: TtkTokenKind;
begin
  if KeyComp('coroutine.status') then Result := tkKeyQuaternary else
  Result := tkIdentifier;
end;

function TSynLuaSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynLuaSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]{$IFDEF FPC}(){$ENDIF}
  else
    Result := tkIdentifier;

  if result=tkIdentifier then
  begin
    if luasyntaxStringHashList.Find(copy(maybe, 1,fstringLen))<>-1 then
      result:=tkInternalFunction;
  end;
end;

procedure TSynLuaSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NullProc;
      #10: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LFProc;
      #13: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CRProc;
      '-': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LuaCommentOpenProc;
      '[': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LuaMStringOpenProc;
      '"': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}String1OpenProc;
      '''': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}String2OpenProc;
      #1..#9, #11, #12, #14..#32 : fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      '0'..'9': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}IdentProc;
    else
      fProcTable[I] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

procedure TSynLuaSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(FTokenEnd);
    if fTokenEnd>length(FLineText) then break;

  until (not (FLineText[FTokenEnd] in [#1..#32]));
end;

procedure TSynLuaSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynLuaSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(FTokenEnd);
  if FLineText[FTokenEnd] = #10 then
    inc(FTokenEnd);
end;

procedure TSynLuaSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(FTokenEnd);
end;

function TSynLuaSyn.LongDelimCheck(aRun: integer): integer;
var
  sep: integer;
begin
  sep:=1;
  while ((aRun+sep)<length(FLineText)) and (FLineText[aRun+sep]='=') and (sep<255) do Inc(sep);
  if ((aRun+sep)<=length(FLineText)) and (FLineText[aRun]=FLineText[aRun+Sep]) then exit(sep);
  result:=0;
end;

procedure TSynLuaSyn.LuaCommentOpenProc;
var sep: Integer;
begin
  Inc(FTokenEnd);
  //check for --[ or --

  if FTokenEnd>length(FLineText) then
  begin
    fTokenID := tkIdentifier;
    exit;
  end;

  if (length(FLineText)>FtokenEnd+1) and (FLineText[FTokenEnd] = '-') and
     (FLineText[FTokenEnd + 1] = '[') then
  begin
    sep:=LongDelimCheck(FTokenEnd+1);
    if sep>0 then
    begin
      Inc(FTokenEnd, sep + 2);
      fRangeExtended := PtrUInt(rsLuaMComment)+10*sep;
      StartCodeFoldBlock;

      LuaMCommentOpenProc;
      exit;
    end;
  end;

  if (FTokenEnd<length(flinetext)) and (FLineText[FTokenEnd] = '-') then   //--
  begin
    fRangeExtended := PtrUInt(0);
    LuaCommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynLuaSyn.LuaCommentProc;
begin
  fTokenID := tkComment;

  FTokenEnd:=length(FLineText)+1;

  fRangeExtended:=0;
end;

procedure TSynLuaSyn.LuaMCommentOpenProc;
begin
  LuaMCommentProc;
  fTokenID := tkComment;
end;

procedure TSynLuaSyn.LuaMCommentProc;
var sep,tmp: Integer;
begin
  if FTokenEnd>length(flinetext) then exit;

  case FLineText[FTokenEnd] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (FLineText[FTokenEnd] = ']') then
        begin
          sep:=LongDelimCheck(FTokenEnd);
          if (sep>0) and (sep=(fRangeExtended div 10)) then
          begin
            tmp:=FTokenPos;
            FTokenPos:=FTokenEnd;
            Inc(FTokenEnd, sep + 1);
            fRangeExtended := PtrUInt(rsUnKnown);

            EndCodeFoldBlock;
            FTokenPos:=tmp;


            Break;
          end;
        end;
        Inc(FTokenEnd);
      until FTokenEnd>length(FLineText);
    end;
  end;
end;

procedure TSynLuaSyn.LuaMStringOpenProc;
var sep: Integer;
begin
  Inc(FTokenEnd);
  sep:=LongDelimCheck(FTokenEnd-1);
  if sep>0 then
  begin
    Inc(FTokenEnd, sep);
    fRangeExtended := ptrUInt(rsLuaMString)+10*sep;
    StartCodeFoldBlock;

    LuaMStringProc;
    fTokenID := tkLuaMString;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynLuaSyn.LuaMStringProc;
var sep,tmp: Integer;
begin
  if FTokenEnd>length(FLineText) then exit;

  case FLineText[FTokenEnd] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkLuaMString;
      repeat
        if (FLineText[FTokenEnd] = ']') then
        begin
          sep:=LongDelimCheck(FTokenEnd);
          if (sep>0) and (sep=(fRangeExtended div 10)) then
          begin
            tmp:=FTokenPos;
            FTokenPos:=FTokenEnd;
            Inc(FTokenEnd, sep + 1);
            fRangeExtended := PtrUInt(rsUnKnown);

            EndCodeFoldBlock;
            FTokenPos:=tmp;


            Break;
          end;
        end;
        Inc(FTokenEnd);
      until FTokenEnd>=length(FLineText)
    end;
  end;
end;

procedure TSynLuaSyn.NumberProc;
var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := FTokenEnd;
  Inc(FTokenEnd);
  fTokenID := tkNumber;
  while (FTokenEnd<=length(FLineText)) and (FLineText[FTokenEnd] in
    ['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+']) do
  begin
    case FLineText[FTokenEnd] of
      '.':
        if (FTokenEnd<Length(FLineText)) and (FLineText[Succ(FTokenEnd)] = '.') then
          Break
        else
          if (fTokenID <> tkHex) then
            fTokenID := tkFloat
          else // invalid
          begin
            fTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not (FLineText[Pred(FTokenEnd)] in ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if (FTokenEnd<Length(FLineText)) and (not (FLineText[Succ(FTokenEnd)] in ['0'..'9', '+', '-'])) then // invalid
          begin
            Inc(FTokenEnd);
            fTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'7':
        if (FTokenEnd = Succ(idx1)) and (FLineText[idx1] = '0') then // octal number
          fTokenID := tkNumber; // Jean-Franois Goulet - Changed for token Number because token Octal was plain text and cannot be modified...
      '8', '9':
        if (FLineText[idx1] = '0') and
           ((fTokenID <> tkHex) and (fTokenID <> tkFloat)) then // invalid octal char
             fTokenID := tkUnknown;
      'a'..'d', 'A'..'D':
        if fTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (fTokenID <> tkHex) then
          if FLineText[Pred(FTokenEnd)] in ['0'..'9'] then // exponent
          begin
            for i := idx1 to Pred(FTokenEnd) do
              if FLineText[i] in ['e', 'E'] then // too many exponents
              begin
                fTokenID := tkUnknown;
                Exit;
              end;
            if (FTokenEnd<Length(FLineText)) and (not (FLineText[Succ(FTokenEnd)] in ['0'..'9', '+', '-'])) then
              Break
            else
              fTokenID := tkFloat
          end
          else // invalid char
            Break;
      'f', 'F':
        if fTokenID <> tkHex then
        begin
          for i := idx1 to Pred(FTokenEnd) do
            if FLineText[i] in ['f', 'F'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
          begin
            if FLineText[Pred(FTokenEnd)] in ['l', 'L'] then // can't mix
              Break;
          end
          else
            fTokenID := tkFloat;
        end;
      'l', 'L':
        begin
          for i := idx1 to Pred(FTokenEnd) do
            if FLineText[i] in ['l', 'L'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
            if FLineText[Pred(FTokenEnd)] in ['f', 'F'] then // can't mix
              Break;
        end;
      'u', 'U':
        if fTokenID = tkFloat then // not allowed
          Break
        else
          for i := idx1 to Pred(FTokenEnd) do
            if FLineText[i] in ['u', 'U'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
      'x', 'X':
        if (FTokenEnd<length(FLineText)) and (FTokenEnd = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLineText[idx1] = '0') and  // 0x...
           (FLineText[Succ(FTokenEnd)] in ['0'..'9', 'a'..'f', 'A'..'F']) then // 0x... must be continued with a number
             fTokenID := tkHex
           else // invalid char
           begin
             if (FTokenEnd<length(FLineText)) and (not Identifiers[FLineText[Succ(FTokenEnd)]]) and
                (FLineText[Succ(idx1)] in ['x', 'X']) then
             begin
               Inc(FTokenEnd); // highlight 'x' too
               fTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(FTokenEnd);
  end; // while
  if (FTokenEnd<=length(FlineText)) and (FLineText[FTokenEnd] in ['A'..'Z', 'a'..'z', '_']) then
    fTokenID := tkUnknown;
end;

procedure TSynLuaSyn.String1OpenProc;
begin
  Inc(FTokenEnd);
  fRangeExtended := PtrUInt(rsString1);
  String1Proc;
  fTokenID := tkString;
end;

procedure TSynLuaSyn.String1Proc;
begin
  fTokenID := tkString;
  repeat

    if (FTokenEnd<=length(FLineText)) and (((FLineText[FTokenEnd] = '"') and (FLineText[FTokenEnd - 1] <> '\')) or ((FLineText[FTokenEnd - 1] = '\') and (FLineText[FTokenEnd - 2] = '\') and (FLineText[FTokenEnd] = '"'))) then
    begin
      Inc(FTokenEnd, 1);
      fRangeExtended := PtrUInt(rsUnKnown);
      Break;
    end;
    Inc(FTokenEnd);
  until ftokenend>length(flinetext);
end;

procedure TSynLuaSyn.String2OpenProc;
begin
  Inc(FTokenEnd);
  fRangeExtended := PtrUInt(rsString2);
  String2Proc;
  fTokenID := tkString;
end;

procedure TSynLuaSyn.String2Proc;
begin
  fTokenID := tkString;
  repeat

    if (ftokenEnd<length(FLineText)) and (FLineText[FTokenEnd] = '''') then
    begin
      Inc(FTokenEnd, 1);
      fRangeExtended := PtrUInt(rsUnKnown);
      Break;
    end;
    Inc(FTokenEnd);
  until FTokenEnd>length(FLineText);
end;

constructor TSynLuaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground:=clBlue;
  AddAttribute(fKeyAttri);

  fKeySecondaryAttri:= TSynHighLighterAttributes.Create(SYNS_AttrReservedWord2);
  fKeySecondaryAttri.Style := [fsBold];
  fKeySecondaryAttri.Foreground:=$c08000;
  AddAttribute(fKeySecondaryAttri);

  fKeyTertiaryAttri:= TSynHighLighterAttributes.Create(SYNS_AttrReservedWord3);
  fKeyTertiaryAttri.Style := [fsBold];
  fKeyTertiaryAttri.Foreground:=$ff0080;
  AddAttribute(fKeyTertiaryAttri);

  fKeyQuaternaryAttri:= TSynHighLighterAttributes.Create(SYNS_AttrReservedWord4);
  fKeyQuaternaryAttri.Style := [fsBold, fsItalic];
  fKeyQuaternaryAttri.Foreground:=$a00000;
  AddAttribute(fKeyQuaternaryAttri);

  fInternalFunctionAttri := TSynHighLighterAttributes.Create(SYNS_AttrInternalFunction);
  fInternalFunctionAttri.Style := [fsBold];
  fInternalFunctionAttri.Foreground:=$c08000;
  fInternalFunctionAttri.Background:=$eeeeee;
  AddAttribute(fInternalFunctionAttri);

  fLuaMStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrLuaMString);
  fLuaMStringAttri.Foreground := $0000ff;
  AddAttribute(fLuaMStringAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := $f00000;
  AddAttribute(fNumberAttri);

  fHexAttri := TSynHighLighterAttributes.Create(SYNS_AttrHexadecimal);
  fHexAttri.Foreground := $708f00;
  AddAttribute(fHexAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := $0505e0;
  AddAttribute(fStringAttri);

  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterLua;
  fRangeExtended := ptrUInt(rsUnknown);
end;

procedure TSynLuaSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;

procedure TSynLuaSyn.IdentProc;
begin
  fTokenID := IdentKind(@FLineText[FTokenEnd]);
  inc(FTokenEnd, fStringLen);
  while (FTokenEnd<length(FLineText)) and Identifiers[FLineText[FTokenEnd]] do
    Inc(FTokenEnd);
end;

procedure TSynLuaSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  if ord(FLineText[FTokenEnd])>$80 then
    inc(FTokenEnd,2)
  else
    inc(FTokenEnd);

  fTokenID := tkUnknown;
end;

procedure TSynLuaSyn.Next;
var
  l: Integer;

  s: string;
begin
  ftokenid:=tkNull;
  StartCodeFold:=false;
  EndCodeFold:=false;

  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  l := length(FLineText);
  If FTokenPos > l then
    // At line end
    exit
  else

  case TRangeState(fRangeExtended mod 10) of
    rsLuaMComment: LuaMCommentProc;
    rsLuaMString: LuaMStringProc;
  else
    begin
      fRangeExtended := PtrUInt(rsUnknown);
      fProcTable[FLineText[FTokenEnd]];
    end;
  end;

  if startcodefold then
    StartCodeFoldBlock;

  if EndCodeFold then
    EndCodeFoldBlock;

        {
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'if') then
    StartCodeFoldBlock;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'do') then
    StartCodeFoldBlock;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'function') then
    StartCodeFoldBlock;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'end') then
    EndCodeFoldBlock;



  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'repeat') then
    StartCodeFoldBlock;

  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = 'until') then
    EndCodeFoldBlock;   }


end;

function TSynLuaSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

function TSynLuaSyn.GetKeyWords: string;
begin
  Result :=
    'and,break,do,dofile,else,elseif,end,exit,false,for,function,if,in,loa' +
    'ddll,local,nil,not,or,print,repeat,return,Sleep,then,true,type,until,w' +
    'hile';
end;

function TSynLuaSyn.GetToken: String;
begin
  Result := copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynLuaSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;
{$ENDIF}

function TSynLuaSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLuaSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkKeySecondary: Result := fKeySecondaryAttri;
    tkKeyTertiary: Result := fKeyTertiaryAttri;
    tkKeyQuaternary: Result := fKeyQuaternaryAttri;
    tkLuaMString: Result := fLuaMStringAttri;
    tkNumber, tkFloat: Result := fNumberAttri;
    tkHex: result := fHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkInternalFunction: result:= fInternalFunctionAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynLuaSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynLuaSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z'];
end;

function TSynLuaSyn.GetSampleSource: string;
begin
  Result := 'Sample source for: '#13#10 +
            'Lua Syntax Parser/Highlighter';
end;

function TSynLuaSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterLua;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynLuaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLua;
end;

procedure TSynLuaSyn.ResetRange;
begin
  //
  inherited ResetRange;
//  FCurRange := 0;
  fRangeExtended := PtrUInt(rsUnknown);
end;

procedure TSynLuaSyn.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
  fRangeExtended := PtrInt(CodeFoldRange.RangeType);

end;

function TSynLuaSyn.GetRange: Pointer;
begin
  //Result := Pointer(fRangeExtended);
  CodeFoldRange.RangeType := Pointer(PtrInt(fRangeExtended));

  result:=inherited GetRange;

  if fRangeExtended<>0 then
  asm
  nop
  end;


end;

function TSynLuaSyn.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  reg: TRegistry;
  i: integer;
begin
  reg:=tregistry.create;
  reg.RootKey:=Rootkey;
  result:=false;
  if reg.OpenKey(Key,false) then
  begin
    result:=true;
    for i:=0 to AttrCount-1 do
      result:=result and Attribute[i].LoadFromRegistry(reg);
  end;

  reg.free;

  DefHighlightChange(self);
end;


initialization
  MakeIdentTable;
  luasyntaxStringHashList:=TStringHashList.Create(true);

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynLuaSyn);
{$ENDIF}

finalization
  freeandnil(luasyntaxStringHashList);

end.

