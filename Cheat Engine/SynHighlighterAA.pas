{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynHighlighterPas.pas,v 1.30 2005/01/28 16:53:24 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net


@abstract(Provides a AutoAssembler syntax highlighter for SynEdit)


}


unit SynHighlighterAA;

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
{$ifdef darwin}
macport,  lcltype,
{$else}
  Windows,
{$endif}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes,
  assemblerunit,
  LuaSyntax,
  SynHighlighterCpp,
  SynHighlighterPas;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkOpcode, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkHex, tkDirec, tkChar,
    tkRegister, tkTryExcept);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsSecondaryHighlighter, rsUnKnown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TAutoAssemblerVersion = (dvAutoAssembler1, dvAutoAssembler2, dvAutoAssembler3, dvAutoAssembler4, dvAutoAssembler5,
    dvAutoAssembler6, dvAutoAssembler7, dvAutoAssembler8, dvAutoAssembler2005);

const
  LastAutoAssemblerVersion = dvAutoAssembler2005;


type
  TSynCustomHighlighterHelper=class helper for TSynCustomHighlighter
  public
    function loadFromRegistryDefault(RootKey: HKEY; Key: string): boolean;
    function loadFromRegistryDefault: boolean;
  end;


type
  TSynAASyn = class(TSynCustomHighlighter)
  private
    fCurrentSecondaryHighlighter: TSynCustomHighlighter;
    fLuaSyntaxHighlighter: TSynLuaSyn;
    fCPPSyntaxHighlighter: TSynCppSyn;
    fFPCSyntaxHighlighter: TSynPasSyn;

    fLineRef: string;
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..222] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fCharAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fOpcodeAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fRegisterAttri: TSynHighlighterAttributes;
    fTryExceptAttri: TSynHighlighterAttributes;
    fAutoAssemblerVersion: TAutoAssemblerVersion;
    fPackageSource: Boolean;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func6: TtkTokenKind; //db
    function Func8: TtkTokenKind; //dd
    function Func9: TtkTokenKind; //ah
    function Func10: TtkTokenKind; //bh
    function Func11: TtkTokenKind; //ch
    function Func12: TtkTokenKind; //dh
    function Func13: TtkTokenKind; //di / al
    function Func14: TtkTokenKind; //bl
    function Func15: TtkTokenKind; //cl
    function Func16: TtkTokenKind; //dl
    function Func18: TtkTokenKind; //edi  / bp /r8-r15
    function Func21: TtkTokenKind; //dq
    function Func23: TtkTokenKind; //ebp
    function Func25: TtkTokenKind; //ax / 25  / dil
    function Func26: TtkTokenKind; //bx //mm#
    function Func27: TtkTokenKind; //cx  / dw
    function Func28: TtkTokenKind; //dx / si
    function Func30: TtkTokenKind; //eax / eip
    function Func31: TtkTokenKind; //ebx / rdi
    function Func32: TtkTokenKind; //ecx
    function Func33: TtkTokenKind; //edx / esi
    function Func35: TtkTokenKind; //sp
    function Func36: TtkTokenKind; //rbp
    function Func39: TtkTokenKind; //enable
    function Func40: TtkTokenKind; //esp
    function Func42: TtkTokenKind; //ends
    function Func43: TtkTokenKind; //alloc /define //rax   /rip /align
    function Func44: TtkTokenKind; //resb
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind; //resd
    function Func47: TtkTokenKind; //spl
    function Func50: TtkTokenKind; //xmm0-15
    function Func52: TtkTokenKind; //dealloc / disable
    function Func53: TtkTokenKind; //rsp
    function Func54: TtkTokenKind; //kalloc
    function Func55: TtkTokenKind; //aobscan
    function Func59: TtkTokenKind; //readmem/resq
    function Func62: TtkTokenKind; //luacall
    function Func65: TtkTokenKind; //resw
    function Func68: TtkTokenKind; //include
    function Func81: TtkTokenKind; //allocnx
    function Func82: TtkTokenKind; //assert //allocxo
    function Func84: TtkTokenKind; //aobscanex
    function Func92: TtkTokenKind; //globalalloc
    function Func99: TtkTokenKind; //reassemble
    function Func101: TtkTokenKind; //fullaccess/loadbinary/struct
    function Func108: TtkTokenKind; //CreateThread
    function Func117: TtkTokenKind; //loadlibrary
    function Func123: TtkTokenKind; //aobscanregion
    function Func124: TtkTokenKind; //endstruct
    function Func125: TtkTokenKind; //aobscanmodule
    function Func180: TtkTokenKind; //createthreadandwait
    function Func187: TtkTokenKind; //registersymbol
    function Func222: TtkTokenKind; //unregistersymbol

    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function getfirsttoken(s: string): string;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure LuaProc;
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
    procedure SetAutoAssemblerVersion(const Value: TAutoAssemblerVersion);
    procedure SetPackageSource(const Value: Boolean);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;

    procedure ResetRange; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    property IdentChars;

    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property OpcodeAttri:TSynHighlighterAttributes read fOpcodeAttri write fOpcodeAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property RegisterAttri: TSynHighlighterAttributes read fRegisterAttri write fRegisterAttri;
    property TryExceptAttri: TSynHighlighterAttributes read fTryExceptAttri write fTryExceptAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property CharAttri: TSynHighlighterAttributes read fCharAttri
      write fCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property AutoAssemblerVersion: TAutoAssemblerVersion read fAutoAssemblerVersion write SetAutoAssemblerVersion
      default LastAutoAssemblerVersion;
    property PackageSource: Boolean read fPackageSource write SetPackageSource default True;
  end;

procedure aa_AddExtraCommand(command:pchar);
procedure aa_RemoveExtraCommand(command:pchar);
function isExtraCommand(token:string): boolean;


implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst, registry, betterControls,mainunit2;
{$ENDIF}



var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

  extraCommands: Tstringlist;

function TSynCustomHighlighterHelper.loadFromRegistryDefault(RootKey: HKEY; Key: string): boolean;
//for highlighters that do not implement loadFromRegistry , like the cpp highlighter
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

  if result=false then
  begin
    if self is TSynCppSyn then //load default C colors
    begin
      with TSynCppSyn(Self) do
      begin
        if ShouldAppsUseDarkMode then
        begin

          CommentAttri.Foreground:=$ff00;
          CommentAttri.Style:=[fsItalic];
          CommentAttri.StyleMask:=[fsItalic];

          IdentifierAttri.Foreground:=$ffff;
          InvalidAttri.Foreground:=$ff;
          NumberAttri.Foreground:=$ffff00;
          DirecAttri.Foreground:=$5024b5;
          KeyAttri.Foreground:=$e1af04;
          KeyAttri.Style:=[fsBold];
          KeyAttri.StyleMask:=[fsBold];
          StringAttri.Foreground:=$c0dcc0;
          SymbolAttri.Foreground:=$c0c0c0;
        end
        else
        begin
          CommentAttri.Foreground:=$808000;
          CommentAttri.Style:=[fsItalic];
          CommentAttri.StyleMask:=[fsItalic];

          IdentifierAttri.Foreground:=$364bc5;
          InvalidAttri.Foreground:=$ff;
          NumberAttri.Foreground:=$ff0000;
          DirecAttri.Foreground:=$5024b5;
          KeyAttri.Foreground:=$800000;
          KeyAttri.Style:=[fsBold];
          KeyAttri.StyleMask:=[fsBold];
          StringAttri.Foreground:=$3de12f;
          SymbolAttri.Foreground:=$808080;

        end;

      end;
    end;

  end;

  DefHighlightChange(self);
end;

function TSynCustomHighlighterHelper.loadFromRegistryDefault: boolean;
begin
  result:=loadFromRegistryDefault(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\CPP Highlighter'+darkmodestring);
end;

procedure aa_AddExtraCommand(command:pchar);
begin
  if extraCommands=nil then
  begin
    extraCommands:=tstringlist.create;
    extraCommands.Duplicates:=dupIgnore;
    extracommands.CaseSensitive:=false;
  end;

  extraCommands.Add(command);
end;

procedure aa_RemoveExtraCommand(command:pchar);
begin
  if extracommands<>nil then
  begin
    if extracommands.IndexOf(command)<>-1 then extracommands.Delete(extracommands.IndexOf(command));
    if extracommands.Count=0 then
      freeandnil(extracommands);
  end;
end;

function isExtraCommand(token: string): boolean;
begin
  result:=false;
  if extracommands<>nil then
    result:=extracommands.IndexOf(token)<>-1;
end;


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

procedure TSynAASyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := {$IFDEF FPC}@{$ENDIF}AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[6] := {$IFDEF FPC}@{$ENDIF}Func6;
  fIdentFuncTable[8] := {$IFDEF FPC}@{$ENDIF}Func8;
  fIdentFuncTable[9] := {$IFDEF FPC}@{$ENDIF}Func9;
  fIdentFuncTable[10] := {$IFDEF FPC}@{$ENDIF}Func10;
  fIdentFuncTable[11] := {$IFDEF FPC}@{$ENDIF}Func11;
  fIdentFuncTable[12] := {$IFDEF FPC}@{$ENDIF}Func12;
  fIdentFuncTable[13] := {$IFDEF FPC}@{$ENDIF}Func13;
  fIdentFuncTable[14] := {$IFDEF FPC}@{$ENDIF}Func14;
  fIdentFuncTable[15] := {$IFDEF FPC}@{$ENDIF}Func15;
  fIdentFuncTable[16] := {$IFDEF FPC}@{$ENDIF}Func16;
  fIdentFuncTable[18] := {$IFDEF FPC}@{$ENDIF}Func18;
  fIdentFuncTable[21] := {$IFDEF FPC}@{$ENDIF}Func21;
  fIdentFuncTable[23] := {$IFDEF FPC}@{$ENDIF}Func23;
  fIdentFuncTable[25] := {$IFDEF FPC}@{$ENDIF}Func25;
  fIdentFuncTable[26] := {$IFDEF FPC}@{$ENDIF}Func26;
  fIdentFuncTable[27] := {$IFDEF FPC}@{$ENDIF}Func27;
  fIdentFuncTable[28] := {$IFDEF FPC}@{$ENDIF}Func28;
  fIdentFuncTable[30] := {$IFDEF FPC}@{$ENDIF}Func30;
  fIdentFuncTable[31] := {$IFDEF FPC}@{$ENDIF}Func31;
  fIdentFuncTable[32] := {$IFDEF FPC}@{$ENDIF}Func32;
  fIdentFuncTable[33] := {$IFDEF FPC}@{$ENDIF}Func33;
  fIdentFuncTable[35] := {$IFDEF FPC}@{$ENDIF}Func35;
  fIdentFuncTable[36] := {$IFDEF FPC}@{$ENDIF}Func36;
  fIdentFuncTable[39] := {$IFDEF FPC}@{$ENDIF}Func39;
  fIdentFuncTable[40] := {$IFDEF FPC}@{$ENDIF}Func40;
  fIdentFuncTable[42] := {$IFDEF FPC}@{$ENDIF}Func42;
  fIdentFuncTable[43] := {$IFDEF FPC}@{$ENDIF}Func43;
  fIdentFuncTable[44] := {$IFDEF FPC}@{$ENDIF}Func44;
  fIdentFuncTable[45] := {$IFDEF FPC}@{$ENDIF}Func45;
  fIdentFuncTable[46] := {$IFDEF FPC}@{$ENDIF}Func46;
  fIdentFuncTable[47] := {$IFDEF FPC}@{$ENDIF}Func47;
  fIdentFuncTable[50] := {$IFDEF FPC}@{$ENDIF}Func50;
  fIdentFuncTable[52] := {$IFDEF FPC}@{$ENDIF}Func52;
  fIdentFuncTable[53] := {$IFDEF FPC}@{$ENDIF}Func53;
  fIdentFuncTable[54] := {$IFDEF FPC}@{$ENDIF}Func54;
  fIdentFuncTable[55] := {$IFDEF FPC}@{$ENDIF}Func55;
  fIdentFuncTable[59] := {$IFDEF FPC}@{$ENDIF}Func59;
  fIdentFuncTable[62] := {$IFDEF FPC}@{$ENDIF}Func62;
  fIdentFuncTable[65] := {$IFDEF FPC}@{$ENDIF}Func65;
  fIdentFuncTable[68] := {$IFDEF FPC}@{$ENDIF}Func68;
  fIdentFuncTable[81] := {$IFDEF FPC}@{$ENDIF}Func81;
  fIdentFuncTable[82] := {$IFDEF FPC}@{$ENDIF}Func82;
  fIdentFuncTable[84] := {$IFDEF FPC}@{$ENDIF}Func84;
  fIdentFuncTable[92] := {$IFDEF FPC}@{$ENDIF}Func92;
  fIdentFuncTable[99] := {$IFDEF FPC}@{$ENDIF}Func99;
  fIdentFuncTable[101] := {$IFDEF FPC}@{$ENDIF}Func101;
  fIdentFuncTable[108] := {$IFDEF FPC}@{$ENDIF}Func108;
  fIdentFuncTable[117] := {$IFDEF FPC}@{$ENDIF}Func117;
  fIdentFuncTable[123] := {$IFDEF FPC}@{$ENDIF}Func123;
  fIdentFuncTable[124] := {$IFDEF FPC}@{$ENDIF}Func124;
  fIdentFuncTable[125] := {$IFDEF FPC}@{$ENDIF}Func125;
  fIdentFuncTable[180] := {$IFDEF FPC}@{$ENDIF}Func180;
  fIdentFuncTable[187] := {$IFDEF FPC}@{$ENDIF}Func187;
  fIdentFuncTable[222] := {$IFDEF FPC}@{$ENDIF}Func222;
end;

function TSynAASyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then inc(ToHash);
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynAASyn.KeyComp(const aKey: string): Boolean;
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

function TSynAASyn.Func6: TtkTokenKind;
var s: string;
begin
  Result := tkIdentifier;
  if KeyComp('db') then
  begin
    //db.  could be db xx xx xx
    //or
    //aobscan(xxx, 00 11 db aa)

    s:=trim(copy(fline, 1, ftokenpos));
    if s='' then //first token
    begin
      s:=lowercase(copy(trim(fToIdent),1,2));
      if (s='db') then
        Result := tkKey;

    end;
  end;

end;

function TSynAASyn.Func8: TtkTokenKind;
var s: string;
begin
  Result := tkIdentifier;
  if KeyComp('dd') then
  begin
    //dd.  could be dd dd
    //or
    //aobscan(xxx, 00 11 dd aa)

    s:=trim(copy(fline, 1, ftokenpos));
    if s='' then //first token
    begin
      s:=lowercase(copy(trim(fToIdent),1,2));
      if (s='dd') then
        Result := tkKey;

    end;
  end;

end;

function TSynAASyn.Func9: TtkTokenKind;
begin
  if KeyComp('ah') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func10: TtkTokenKind;
begin
  if KeyComp('bh') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func11: TtkTokenKind;
begin
  if KeyComp('ch') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func12: TtkTokenKind;
begin
  if KeyComp('dh') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func13: TtkTokenKind;
begin
  if KeyComp('di') then Result := tkRegister else
    if KeyComp('al') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TSynAASyn.Func14: TtkTokenKind;
begin
  if KeyComp('bl') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func15: TtkTokenKind;
begin
  if KeyComp('cl') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func16: TtkTokenKind;
begin
  if KeyComp('dl') then Result := tkRegister else
    Result := tkIdentifier;
end;


function TSynAASyn.Func18: TtkTokenKind;
begin
  if KeyComp('edi') then Result := tkRegister else
    if KeyComp('bp') then Result := tkRegister else
    {$ifdef cpu64}
    if KeyComp('r8') then Result := tkRegister else
    if KeyComp('r9') then Result := tkRegister else
    if KeyComp('r10') then Result := tkRegister else
    if KeyComp('r11') then Result := tkRegister else
    if KeyComp('r12') then Result := tkRegister else
    if KeyComp('r13') then Result := tkRegister else
    if KeyComp('r14') then Result := tkRegister else
    if KeyComp('r15') then Result := tkRegister else
    {$endif}
      Result := tkIdentifier;
end;

function TSynAASyn.Func21: TtkTokenKind;
begin
  if KeyComp('dq') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func23: TtkTokenKind;
begin
  if KeyComp('ebp') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func25: TtkTokenKind;
begin
  if KeyComp('ax') then Result := tkRegister else
    if KeyComp('ip') then Result := tkRegister else
      {$ifdef cpu64}
      if KeyComp('dil') then Result := tkRegister else
      {$endif}
        Result := tkIdentifier;
end;

function TSynAASyn.Func26: TtkTokenKind;
begin
  if KeyComp('bx') then Result := tkRegister else
  if KeyComp('mm0') then Result := tkRegister else
  if KeyComp('mm1') then Result := tkRegister else
  if KeyComp('mm2') then Result := tkRegister else
  if KeyComp('mm3') then Result := tkRegister else
  if KeyComp('mm4') then Result := tkRegister else
  if KeyComp('mm5') then Result := tkRegister else
  if KeyComp('mm6') then Result := tkRegister else
  if KeyComp('mm7') then Result := tkRegister else
{$ifdef cpu64}
  if KeyComp('mm8') then Result := tkRegister else
  if KeyComp('mm9') then Result := tkRegister else
  if KeyComp('mm10') then Result := tkRegister else
  if KeyComp('mm11') then Result := tkRegister else
  if KeyComp('mm12') then Result := tkRegister else
  if KeyComp('mm13') then Result := tkRegister else
  if KeyComp('mm14') then Result := tkRegister else
  if KeyComp('mm15') then Result := tkRegister else
{$endif}

    Result := tkIdentifier;
end;

function TSynAASyn.Func27: TtkTokenKind;
begin
  if KeyComp('cx') then Result := tkRegister else
    if KeyComp('dw') then Result := tkKey else
      Result := tkIdentifier;
end;

function TSynAASyn.Func28: TtkTokenKind;
begin
  if KeyComp('dx') then Result := tkRegister else
    if KeyComp('si') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TSynAASyn.Func30: TtkTokenKind;
begin
  if KeyComp('eax') then Result := tkRegister else
    if KeyComp('eip') then Result := tkRegister else
    {$ifdef cpu64}
      if KeyComp('bpl') then Result := tkRegister else
    {$endif}
        Result := tkIdentifier;
end;

function TSynAASyn.Func31: TtkTokenKind;
begin
  if KeyComp('ebx') then Result := tkRegister else
  {$ifdef cpu64}
    if KeyComp('rdi') then Result := tkRegister else
  {$endif}
      Result := tkIdentifier;
end;

function TSynAASyn.Func32: TtkTokenKind;
begin
  if KeyComp('Label') then Result := tkKey else
    if KeyComp('ecx') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TSynAASyn.Func33: TtkTokenKind;
begin
  if KeyComp('edx') then Result := tkRegister else
    if KeyComp('esi') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TSynAASyn.Func35: TtkTokenKind;
begin
  if KeyComp('sp') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TSynAASyn.Func36: TtkTokenKind;
begin
  {$ifdef cpu64}
  if KeyComp('rbp') then Result := tkRegister else
  {$endif}
    Result := tkIdentifier;
end;

function TSynAASyn.Func39: TtkTokenKind; //enable
begin
  if KeyComp('enable') then Result := tkspace else
    if KeyComp('jmp1') then Result := tkopcode else
      Result := tkIdentifier;
end;

function TSynAASyn.Func40: TtkTokenKind; //esp/sil
begin
  if KeyComp('esp') then Result := tkRegister else
  {$ifdef cpu64}
    if KeyComp('sil') then Result := tkRegister else
  {$endif}
      Result := tkIdentifier;
end;

function TSynAASyn.Func42: TtkTokenKind; //ends
begin
  if KeyComp('ends') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func43: TtkTokenKind; //alloc /define
begin
  {$ifdef cpu64}
  if KeyComp('rax') then Result := tkRegister else
    if KeyComp('rip') then Result := tkRegister else
  {$endif}
      if KeyComp('alloc') then Result := tkKey else
        if KeyComp('define') then Result := tkKey else
          if KeyComp('align') then Result := tkKey else
            Result := tkIdentifier;
end;

function TSynAASyn.Func44: TtkTokenKind; //rbx
begin
  {$ifdef cpu64}
  if KeyComp('rbx') then Result := tkRegister else
  {$endif}
  if KeyComp('resb') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func45: TtkTokenKind; //rcx
begin
  {$ifdef cpu64}
  if KeyComp('rcx') then Result := tkRegister else
  {$endif}
    Result := tkIdentifier;
end;

function TSynAASyn.Func46: TtkTokenKind; //rdx
begin
  {$ifdef cpu64}
  if KeyComp('rdx') then Result := tkRegister else
  if KeyComp('rsi') then Result := tkRegister else
  {$endif}
  if KeyComp('resd') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func47: TtkTokenKind; //spl
begin
  {$ifdef cpu64}
  if KeyComp('spl') then Result := tkRegister else
  {$endif}
    Result := tkIdentifier;
end;

function TSynAASyn.Func50: TtkTokenKind; //xmm0
begin
  if KeyComp('xmm0') then Result := tkRegister else
  if KeyComp('xmm1') then Result := tkRegister else
  if KeyComp('xmm2') then Result := tkRegister else
  if KeyComp('xmm3') then Result := tkRegister else
  if KeyComp('xmm4') then Result := tkRegister else
  if KeyComp('xmm5') then Result := tkRegister else
  if KeyComp('xmm6') then Result := tkRegister else
  if KeyComp('xmm7') then Result := tkRegister else
{$ifdef cpu64}
  if KeyComp('xmm8') then Result := tkRegister else
  if KeyComp('xmm9') then Result := tkRegister else
  if KeyComp('xmm10') then Result := tkRegister else
  if KeyComp('xmm11') then Result := tkRegister else
  if KeyComp('xmm12') then Result := tkRegister else
  if KeyComp('xmm13') then Result := tkRegister else
  if KeyComp('xmm14') then Result := tkRegister else
  if KeyComp('xmm15') then Result := tkRegister else
{$endif}
    Result := tkIdentifier;
end;


function TSynAASyn.Func52: TtkTokenKind; //dealloc
begin
  if KeyComp('dealloc') then Result := tkKey else
    if KeyComp('disable') then Result := tkspace else
    Result := tkIdentifier;
end;

function TSynAASyn.Func53: TtkTokenKind; //rsp
begin
  {$ifdef cpu64}
  if KeyComp('rsp') then Result := tkRegister else
  {$endif}
    Result := tkIdentifier;
end;

function TSynAASyn.Func54: TtkTokenKind; //kalloc
begin
  if KeyComp('kalloc') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func55: TtkTokenKind; //aobscan
begin
  if KeyComp('aobscan') then Result := tkKey else
    Result := tkIdentifier;
end;


function TSynAASyn.Func59: TtkTokenKind; //readmem /resq
begin
  if KeyComp('readmem') then Result := tkKey else
  if KeyComp('resq') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func62: TtkTokenKind; //include
begin
  if KeyComp('luacall') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func65: TtkTokenKind; //resw
begin
  if KeyComp('resw') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func68: TtkTokenKind; //include
begin
  if KeyComp('include') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func81: TtkTokenKind; //allocnx
begin
  if KeyComp('allocnx') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func82: TtkTokenKind; //include
begin
  if KeyComp('assert') then Result := tkKey else
    if KeyComp('allocxo') then Result := tkKey else
      Result := tkIdentifier;
end;

function TSynAASyn.Func84: TtkTokenKind; //aobscanex
begin
  if KeyComp('aobscanex') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func92: TtkTokenKind; //globalalloc
begin
  if KeyComp('globalalloc') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func99: TtkTokenKind; //reassemble
begin
  if KeyComp('reassemble') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func101: TtkTokenKind;
begin
  if KeyComp('loadbinary') then Result := tkKey else
    if KeyComp('fullaccess') then Result := tkKey else
      if KeyComp('struct') then Result := tkKey else
        Result := tkIdentifier;
end;

function TSynAASyn.Func108: TtkTokenKind; //CreateThread
begin
  if KeyComp('createthread') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func117: TtkTokenKind; //loadlibrary
begin
  if KeyComp('loadlibrary') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func123: TtkTokenKind; //aobscanregion
begin
  if KeyComp('aobscanregion') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func124: TtkTokenKind; //endstruct
begin
  if KeyComp('endstruct') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func125: TtkTokenKind; //aobscanmodule
begin
  if KeyComp('aobscanmodule') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func180: TtkTokenKind; //createthreadandwait
begin
  if KeyComp('createthreadandwait') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func187: TtkTokenKind; //registersymbol
begin
  if KeyComp('registersymbol') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.Func222: TtkTokenKind; //unregistersymbol
begin
  if KeyComp('unregistersymbol') then Result := tkKey else
    Result := tkIdentifier;
end;

function TSynAASyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynAASyn.getfirsttoken(s: string): string;
var i: integer;
begin
  result:=s;
  for i:=1 to length(s) do
  begin
    if (s[i]='(') or (s[i]=' ') or (s[i]=#9) or (s[i]=',') or (s[i]=#10) or (s[i]=#13) then
    begin
      result:=copy(s,1,i-1);
      exit;
    end;
  end;
end;

function TSynAASyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
  ft: string;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 223 then Result := fIdentFuncTable[HashKey]{$IFDEF FPC}(){$ENDIF}  else
    Result := tkIdentifier;

    
  if (result=tkIdentifier) then
  begin
    ft:=getfirsttoken(maybe);
    if GetOpcodesIndex(ft)<>-1 then
      result:=tkOpcode
    else
    if isExtraCommand(ft) then
      result:=tkKey;
  end;


end;

procedure TSynAASyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NullProc;
      #10: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LFProc;
      #13: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      '#': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}IntegerProc;
      #39: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}StringProc;
      '0'..'9','A'..'F','a'..'f': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      'G'..'Z', 'g'..'z', '_':
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}IdentProc;
      '{': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}BraceOpenProc;
      '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}RoundOpenProc;
            '.': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}PointProc;
            ';': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SemicolonProc;
            '/': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SlashProc;
            ':', '>': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}ColonOrGreaterProc;
            '<': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LowerProc;
            '@': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}AddressOpProc;
          else
            fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SymbolProc;
          end;
        end;
    else
      fProcTable[I] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSynAASyn.Create(AOwner: TComponent);
begin
 // OutputDebugString('constructor TSynAASyn.Create(AOwner: TComponent);');
  inherited Create(AOwner);
  fAutoAssemblerVersion := LastAutoAssemblerVersion;
  fPackageSource := True;

  fAsmAttri := TSynHighlighterAttributes.Create('{$ASM}');
  fAsmAttri.Foreground:=clred;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  if ShouldAppsUseDarkMode() then
    fCommentAttri.Foreground:=$ff7f00
  else
    fCommentAttri.Foreground:=clBlue;

  AddAttribute(fCommentAttri);

  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective);
  fDirecAttri.Style:= [fsItalic];
  AddAttribute(fDirecAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fOpcodeAttri := TSynHighlighterAttributes.Create('Mnemonic');
  fOpcodeAttri.Style:= [fsBold];
  AddAttribute(fOpcodeAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground:=$101080;
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);

  fRegisterAttri := TSynHighlighterAttributes.Create('Register');
  fRegisterAttri.Style:= [fsBold];
  fRegisterAttri.Foreground:=$0080f0;
  AddAttribute(fRegisterAttri);

  fTryExceptAttri := TSynHighlighterAttributes.Create('Try/Except');
  fTryExceptAttri.Style:= [fsBold, fsUnderline];
  fTryExceptAttri.Foreground:=$0080f0;
  AddAttribute(fTryExceptAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground:=clGreen;
  AddAttribute(fNumberAttri);

  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal);
  fHexAttri.Foreground:=clGreen;
  AddAttribute(fHexAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  if ShouldAppsUseDarkMode then
    fSpaceAttri.Foreground:=$9f7f1f
  else
    fSpaceAttri.Foreground:=clNavy;
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground:=clRed;

  AddAttribute(fStringAttri);
  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter);
//  fCharAttri.Foreground:=clRed;
  AddAttribute(fCharAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
end; { Create }

procedure TSynAASyn.SetLine(const NewValue: string; LineNumber:Integer);
begin
  if fRange=rsSecondaryHighlighter then
    fCurrentSecondaryHighlighter.SetLine(NewValue, LineNumber);


  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  Run := 0;
  fLineNumber := LineNumber;

  if fRange<>rsSecondaryHighlighter then //prevent a double next
    Next;
end; { SetLine }

procedure TSynAASyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynAASyn.AsciiCharProc;
begin
  fTokenID := tkChar;
  Inc(Run);
  while FLine[Run] in ['0'..'9', '$', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end;

procedure TSynAASyn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if fRange in [rsDirective, rsDirectiveAsm] then
        fTokenID := tkDirec
      else
        fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then
        begin
          Inc(Run);
          if fRange in [rsBorAsm, rsDirectiveAsm] then
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

procedure TSynAASyn.LuaProc;
begin
  fTokenID := tkComment;
  if uppercase(fLine)='{$ASM}' then
  begin
    inc(run,6);
    fTokenID := tkAsm;
    fRange:=rsUnKnown;
  end
  else
  begin

    fCurrentSecondaryHighlighter.Next;


   { case fLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;

      else
      repeat
        Inc(Run);


      until fLine[Run] in [#0, #10, #13];
    end;  }


  end;
end;

procedure TSynAASyn.BraceOpenProc;
var i,l: integer;
  changeHighlighter: (chlNo, chlLua, chlPascal, chlCPP);
  braceend: integer;
begin
  changeHighlighter:=chlNo;
  braceend:=0;

  l:=StrLen(fLine);
  //check for syntax highlighter changes
  if (Run=0) and
     (l>=6) and (fLine[1] = '$') and   //{$LUA}
     (uppercase(fLine[2]) = 'L') and
     (uppercase(fLine[3]) = 'U') and
     (uppercase(fLine[4]) = 'A') then
  begin
    braceend:=length(fline);
    if (fLine[5] = '}') then
    begin
      changeHighlighter:=chlLua;
      braceend:=5;
    end
    else
    begin
      //could be {$LuaCode  xxxx=xxxx yyyy=yyyy() zzzz=[zzzzz]}   or  just {$luacode}
      if l>=9 then
      begin
        if (uppercase(fLine[5])='C') and
           (uppercase(fLine[6])='O') and
           (uppercase(fLine[7])='D') and
           (uppercase(fLine[8])='E') then
        begin
          if (fLine[9] in ['}',' ']) then
            for i:=9 to l do
              if fline[i]='}' then
              begin
                changeHighlighter:=chlLua;
                braceend:=i;
                break;
              end;
        end;
      end;
    end;
  end;

  if (Run=0) and
     (l>=8) and (fLine[1] = '$') and   //{$CCODE}
     (uppercase(fLine[2]) = 'C') and
     (uppercase(fLine[3]) = 'C') and
     (uppercase(fLine[4]) = 'O') and
     (uppercase(fLine[5]) = 'D') and
     (uppercase(fLine[6]) = 'E') then
  begin
    if (fLine[7] in ['}',' ']) then
      for i:=7 to l do
        if fline[i]='}' then
        begin
          changeHighlighter:=chlCPP;
          braceend:=i;
          break;
        end;
  end;

  if (Run=0) and
     (l>=4) and (fLine[1] = '$') and   //{$C}
     (uppercase(fLine[2]) = 'C') then
  begin
    //{$C} block
    if (fLine[3] in ['}',' ']) then
      for i:=3 to l do
        if fline[i]='}' then
        begin
          changeHighlighter:=chlCPP;
          braceend:=i;
          break;
        end;
  end;


  if changeHighlighter<>chlNo then
  begin
    inc(run,braceend);
    FTokenID:=tkAsm;

    //just in case I add {$pascode} and {$cppcode}
    case changeHighlighter of
      chlLua:
      begin
        if fLuaSyntaxHighlighter=nil then
        begin
          fLuaSyntaxHighlighter:=TSynLuaSyn.Create(self);
          fLuaSyntaxHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\Lua Highlighter'+darkmodestring);
        end;
        fCurrentSecondaryHighlighter:=fLuaSyntaxHighlighter;
      end;

      chlPascal:
      begin
        if fFPCSyntaxHighlighter=nil then
        begin
          fFPCSyntaxHighlighter:=TSynPasSyn.Create(self);
          fFPCSyntaxHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\Pascal Highlighter'+darkmodestring);
        end;
        fCurrentSecondaryHighlighter:=fFPCSyntaxHighlighter;
      end;

      chlCPP:
      begin
        if fCPPSyntaxHighlighter=nil then
        begin
          fCPPSyntaxHighlighter:=TSynCppSyn.Create(self);


          fCPPSyntaxHighlighter.loadFromRegistryDefault(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\CPP Highlighter'+darkmodestring);

        end;
        fCurrentSecondaryHighlighter:=fCPPSyntaxHighlighter;
      end;
    end;

    fCurrentSecondaryHighlighter.AttachToLines(CurrentLines);
    fCurrentSecondaryHighlighter.CurrentLines:=CurrentLines;
    fCurrentSecondaryHighlighter.StartAtLineIndex(fLineNumber);

    fRange := rsSecondaryHighlighter;
    exit;
  end;


  if (Run=0) and (l>=6) and (fLine[Run + 1] = '$') and   //{$ASM}
     (uppercase(fLine[Run + 2]) = 'A') and
     (uppercase(fLine[Run + 3]) = 'S') and
     (uppercase(fLine[Run + 4]) = 'M') and
     (fLine[Run + 5] = '}')
  then
  begin
    FTokenID:=tkIdentifier;
    inc(run,5);
    exit;
  end
  else
  if (Run=0) and (l>=9) and (fLine[Run + 1] = '$') and   //{$STRICT}
     (uppercase(fLine[Run + 2]) = 'S') and
     (uppercase(fLine[Run + 3]) = 'T') and
     (uppercase(fLine[Run + 4]) = 'R') and
     (uppercase(fLine[Run + 5]) = 'I') and
     (uppercase(fLine[Run + 6]) = 'C') and
     (uppercase(fLine[Run + 7]) = 'T') and
     (fLine[Run + 8] = '}')
  then
  begin
    FTokenID:=tkIdentifier;
    inc(run,8);
    exit;
  end
  else
  if (Run=0) and (l>=6) and (fLine[Run + 1] = '$') and   //{$TRY}
    (uppercase(fLine[Run + 2]) = 'T') and
    (uppercase(fLine[Run + 3]) = 'R') and
    (uppercase(fLine[Run + 4]) = 'Y') and
    (fLine[Run + 5] = '}')
  then
  begin
    FTokenID:=tkTryExcept;
    inc(run,6);
    exit;
  end
  else
  if (Run=0) and (l>=9) and (fLine[Run + 1] = '$') and   //{$EXCEPT}
    (uppercase(fLine[Run + 2]) = 'E') and
    (uppercase(fLine[Run + 3]) = 'X') and
    (uppercase(fLine[Run + 4]) = 'C') and
    (uppercase(fLine[Run + 5]) = 'E') and
    (uppercase(fLine[Run + 6]) = 'P') and
    (uppercase(fLine[Run + 7]) = 'T') and
    (fLine[Run + 8] = '}')
  then
  begin
    FTokenID:=tkTryExcept;
    inc(run,9);
    exit;
  end
  else
  begin
    if fRange = rsAsm then
      fRange := rsBorAsm
    else
      fRange := rsBor;

  end;
  BorProc;
 end;

procedure TSynAASyn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynAASyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end; { CRProc }


procedure TSynAASyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end; { IdentProc }


procedure TSynAASyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkHex;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end; { IntegerProc }


procedure TSynAASyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end; { LFProc }


procedure TSynAASyn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '>'] then
    Inc(Run);
end; { LowerProc }


procedure TSynAASyn.NullProc;
begin
  fTokenID := tkNull;
end; { NullProc }

procedure TSynAASyn.NumberProc;
begin
 { Inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E', '-', '+'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkFloat;
      'e', 'E': fTokenID := tkFloat;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // arithmetic
            Break;
          if not (FLine[Run - 1] in ['e', 'E']) then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;   }
    fTokenID := IdentKind((fLine + Run));

  if fTokenID=tkIdentifier then
  begin
    inc(Run);
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', 'a'..'f' , 'A'..'F'] do
    begin
      {case FLine[Run] of
        '.':
          if FLine[Run + 1] = '.' then break;
      end;   }
      inc(Run);
    end;

    if ((FLine[Run]>'G') and (FLine[Run]<='Z')) or ((FLine[Run]>='g') and (FLine[Run]<='z')) then
      fTokenID:=tkIdentifier;
  end
  else
  begin
    inc(Run, fStringLen);
    while Identifiers[fLine[Run]] do inc(Run);
  end;
end; { NumberProc }

procedure TSynAASyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['.', ')'] then
    Inc(Run);
end; { PointProc }

procedure TSynAASyn.AnsiProc;
begin
{  case fLine[Run] of
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
  end;   }
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
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

procedure TSynAASyn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
  {  '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not (fLine[Run] in [#0, #10, #13]) then
          AnsiProc;
      end; }
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynAASyn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange in [rsProperty, rsExports] then
    fRange := rsUnknown;
end;

procedure TSynAASyn.SlashProc;
begin
 { Inc(Run);
  if (fLine[Run] = '/') and (fAutoAssemblerVersion > dvAutoAssembler1) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end
  else if (fLine[Run] = '*') then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
      
    until fLine[Run] in [#0];
  end else fTokenID := tkSymbol;  }

  Inc(Run);
  if fLine[Run] = '/' then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end
  else
  if fline[run] = '*' then
  begin
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
  end 
  else fTokenID := tkSymbol;  

end;

procedure TSynAASyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynAASyn.StringProc;
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

procedure TSynAASyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynAASyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}

  fTokenID := tkUnknown;
  if ord(fline[run])>$80 then  //utf8
    inc(Run,2)
  else
    inc(run);
end;

procedure TSynAASyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
      BorProc;
    rsSecondaryHighlighter:
      LuaProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynAASyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  if fRange=rsSecondaryHighlighter then
  begin
    if fCurrentSecondaryHighlighter is TSynLuaSyn then exit(TSynLuaSyn(fCurrentSecondaryHighlighter).GetDefaultAttribute(index));
    if fCurrentSecondaryHighlighter is TSynCppSyn then exit(TSynCppSyn(fCurrentSecondaryHighlighter).GetDefaultAttribute(index));
    if fCurrentSecondaryHighlighter is TSynPasSyn then exit(TSynPasSyn(fCurrentSecondaryHighlighter).GetDefaultAttribute(index));
    exit(nil);
  end;

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

function TSynAASyn.GetEol: Boolean;
begin
  if fRange=rsSecondaryHighlighter then
    result:=fCurrentSecondaryHighlighter.GetEol
  else
    Result := fTokenID = tkNull;
end;
    {
function TSynAASyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;   }

function TSynAASyn.GetToken: String;
var
  Len: LongInt;
begin
  if frange=rsSecondaryHighlighter then
  begin
    result:=fCurrentSecondaryHighlighter.GetToken;
    exit;
  end;

  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynAASyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  if fRange=rsSecondaryHighlighter then
  begin
    fCurrentSecondaryHighlighter.GetTokenEx(tokenstart, TokenLength);

    if uppercase(tokenstart)='{$ASM}' then
    begin
      tokenlength:=0;
      fRange:=rsANil;
    end;

    exit;
  end;


  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}

function TSynAASyn.GetTokenID: TtkTokenKind;
begin
  if frange=rsSecondaryHighlighter then
  begin
    if fCurrentSecondaryHighlighter is TSynLuaSyn then exit(TtkTokenKind(TSynLuaSyn(fCurrentSecondaryHighlighter).GetTokenID));
    if fCurrentSecondaryHighlighter is TSynCppSyn then exit(TtkTokenKind(TSynCppSyn(fCurrentSecondaryHighlighter).GetTokenID));
    if fCurrentSecondaryHighlighter is TSynPasSyn then exit(TtkTokenKind(TSynPasSyn(fCurrentSecondaryHighlighter).GetTokenID));
    exit(TtkTokenKind(0));
  end;

  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynAASyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if fRange=rsSecondaryHighlighter then
    exit(fCurrentSecondaryHighlighter.GetTokenAttribute);

  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirec: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkOpcode: Result := fOpcodeAttri;
    tkKey: Result := fKeyAttri;
    tkRegister: Result := fRegisterAttri;
    tkNumber: Result := fNumberAttri;
    tkHex: Result := fHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkChar: Result := fCharAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTryExcept: result :=fTryExceptAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := fCommentAttri; //nil;
  end;
end;

function TSynAASyn.GetTokenKind: integer;
begin
  if frange=rsSecondaryHighlighter then
    result:=fCurrentSecondaryHighlighter.GetTokenKind
  else
    Result := Ord(GetTokenID);
end;

function TSynAASyn.GetTokenPos: Integer;
begin
  if frange=rsSecondaryHighlighter then
    result:=fCurrentSecondaryHighlighter.GetTokenPos
  else
    Result := fTokenPos;
end;

function TSynAASyn.GetRange: Pointer;
begin
  if frange=rsSecondaryHighlighter then
    result := pointer(PtruInt(fCurrentSecondaryHighlighter.GetRange)+$1000)
  else
    Result := Pointer(PtruInt(fRange));
end;

procedure TSynAASyn.SetRange(Value: Pointer);
begin
  if ptruint(value) >= $1000 then //lua
  begin
    fCurrentSecondaryHighlighter.SetRange(pointer(ptruint(value)-$1000));
    frange:=rsSecondaryHighlighter;
  end
  else
    fRange := TRangeState(PtrUInt(Value));
end;

procedure TSynAASyn.ResetRange;
begin
  //if frange=rsSecondaryHighlighter then
  //  fCurrentSecondaryHighlighter.ResetRange
 // else
    fRange:= rsUnknown;

end;

function TSynAASyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynAASyn.GetSampleSource: string;
begin
  Result :=  'NYI'#13#10;
end; { GetSampleSource }


class function TSynAASyn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynAASyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynAASyn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynAASyn.SetAutoAssemblerVersion(const Value: TAutoAssemblerVersion);
begin
  if fAutoAssemblerVersion <> Value then
  begin
    fAutoAssemblerVersion := Value;
    if (fAutoAssemblerVersion < dvAutoAssembler3) and fPackageSource then
      fPackageSource := False;
    DefHighlightChange( Self );
  end;
end;


procedure TSynAASyn.SetPackageSource(const Value: Boolean);
begin
  if fPackageSource <> Value then
  begin
    fPackageSource := Value;
    if fPackageSource and (fAutoAssemblerVersion < dvAutoAssembler3) then
      fAutoAssemblerVersion := dvAutoAssembler3;
    DefHighlightChange( Self );
  end;
end;

function TSynAASyn.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
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

  if fLuaSyntaxHighlighter<>nil then
    fLuaSyntaxHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\Lua Highlighter'+darkmodestring);  //perhaps make this a var

  if fCPPSyntaxHighlighter<>nil then
    fCPPSyntaxHighlighter.loadFromRegistryDefault(HKEY_CURRENT_USER, '\Software\'+strCheatEngine+'\CPP Highlighter'+darkmodestring);  //perhaps make this a var

  DefHighlightChange(self);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynAASyn);
{$ENDIF}
end.

