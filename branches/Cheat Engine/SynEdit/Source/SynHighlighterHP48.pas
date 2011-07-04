{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHP48.pas, released 2000-06-23.
The Original Code is based on the cbHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Cyrille de Brebisson.
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

$Id: SynHighlighterHP48.pas,v 1.11.2.1 2007/04/19 06:49:20 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - small memory leak in TSpeedStringList has to be fixed
-------------------------------------------------------------------------------}
{
@abstract(Provides SynEdit with a HP48 assembler syntax highlighter.)
@author(Cyrille de Brebisson <cyrille_de-brebisson@aus.hp.com>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1998-12, converted to SynEdit 2000-06-23)
@lastmod(2000-06-23)
The unit SynHighlighterHP48 provides SynEdit with a HP48 assembler highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERHP48}
unit SynHighlighterHP48;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Graphics,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

const
  NbSubList = 128;

type
  TSpeedStringList = class;

  TSpeedListObject = class
  protected
    FName: string;
    FSpeedList: TSpeedStringList;
    fobject: tobject;
    procedure SetName(const Value: string); virtual;
  public
    property Name: string read FName write SetName;
    constructor Create(name: string);
    destructor Destroy; override;
    property SpeedList: TSpeedStringList read FSpeedList write FSpeedList;
    property pointer: tobject read fobject write fobject;
  end;

  PSpeedListObjects = ^TSpeedListObjects;
  TSpeedListObjects = array[0..MaxListSize] of TSpeedListObject;

  TSpeedStringList = class
  private
    function GetText: string;
    procedure SetText(const Value: string);
    function GetInObject(Index: Integer): TObject;
    procedure SetInObject(Index: Integer; const Value: TObject);
  protected
    FOnChange: TNotifyEvent;
    SumOfUsed: array[0..NbSubList - 1] of integer;
    datasUsed: array[0..NbSubList - 1] of integer;
    datas: array[0..NbSubList - 1] of PSpeedListObjects;
    lengthDatas: array[0..NbSubList - 1] of integer;
    procedure Changed; virtual;
    function Get(Index: Integer): string; virtual;
    function GetObject(Index: Integer): TSpeedListObject;
    function GetCount: integer;
    function GetStringList: TStrings;
    procedure SetStringList(const value: TStrings);
  public
    procedure NameChange(const obj: TSpeedListObject; const NewName: string);
    procedure ObjectDeleted(const obj: TSpeedListObject);

    destructor Destroy; override;
    constructor create;
    function AddObj(const Value: TSpeedListObject): Integer;
    function Add(const Value: string): TSpeedListObject;
    procedure Clear;
    function Find(const name: string): TSpeedListObject;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Objects[Index: Integer]: TSpeedListObject read GetObject;
    property inobject[Index: Integer]: TObject read GetInObject write SetInObject;
    property Strings[Index: Integer]: string read Get; default;
    property count: integer read GetCount;
    property StringList: TStrings read GetStringList write SetStringList;
    property text: string read GetText write SetText;
  end;

  TtkTokenKind = (tkNull, tkAsmKey, tkAsm, tkAsmComment, tksAsmKey, tksAsm,
    tksAsmComment, tkRplKey, tkRpl, tkRplComment);

  TRangeState = (rsRpl, rsComRpl, rssasm1, rssasm2, rssasm3, rsAsm, rsComAsm2,
    rsComAsm1);

  TSynHP48Syn = class(TSynCustomHighLighter)
  private
    fTockenKind: TtkTokenKind;
    fRange: TRangeState;
    fLine: string;
    Run: LongInt;
    fTokenPos: Integer;
    fEol: Boolean;
    Attribs: array[TtkTokenKind] of TSynHighlighterAttributes;
    FRplKeyWords: TSpeedStringList;
    FAsmKeyWords: TSpeedStringList;
    FSAsmNoField: TSpeedStringList;
    FBaseRange: TRangeState;
    function GetAttrib(Index: integer): TSynHighlighterAttributes;
    procedure SetAttrib(Index: integer; Value: TSynHighlighterAttributes);

    function NullProc: TtkTokenKind;
    function SpaceProc: TtkTokenKind;
    function ParOpenProc: TtkTokenKind;
    function RplComProc: TtkTokenKind;
    function AsmComProc(c: char): TtkTokenKind;
    function PersentProc: TtkTokenKind;
    function IdentProc: TtkTokenKind;
    function SlashProc: TtkTokenKind;
    function SasmProc1: TtkTokenKind;
    function SasmProc2: TtkTokenKind;
    function SasmProc3: TtkTokenKind;
    procedure EndOfToken;
    procedure SetHighLightChange;
    function Next1: TtkTokenKind;
    procedure Next2(tkk: TtkTokenKind);
    function GetTokenFromRange: TtkTokenKind;
    function StarProc: TtkTokenKind;
  protected
    function GetAttribCount: integer; override;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    function GetToken: string; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;

    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    {$IFNDEF SYN_CLX}
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
    property AsmKeyWords: TSpeedStringList read FAsmKeyWords;
    property SAsmFoField: TSpeedStringList read FSAsmNoField;
    property RplKeyWords: TSpeedStringList read FRplKeyWords;
  published
    property AsmKey: TSynHighlighterAttributes index Ord(tkAsmKey)
      read GetAttrib write SetAttrib;
    property AsmTxt: TSynHighlighterAttributes index Ord(tkAsm)
      read GetAttrib write SetAttrib;
    property AsmComment: TSynHighlighterAttributes index Ord(tkAsmComment)
      read GetAttrib write SetAttrib;
    property sAsmKey: TSynHighlighterAttributes index Ord(tksAsmKey)
      read GetAttrib write SetAttrib;
    property sAsmTxt: TSynHighlighterAttributes index Ord(tksAsm)
      read GetAttrib write SetAttrib;
    property sAsmComment: TSynHighlighterAttributes index Ord(tksAsmComment)
      read GetAttrib write SetAttrib;
    property RplKey: TSynHighlighterAttributes index Ord(tkRplKey)
      read GetAttrib write SetAttrib;
    property RplTxt: TSynHighlighterAttributes index Ord(tkRpl)
      read GetAttrib write SetAttrib;
    property RplComment: TSynHighlighterAttributes index Ord(tkRplComment)
      read GetAttrib write SetAttrib;
    property BaseRange: TRangeState read FBaseRange write FBaseRange;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  tkTokenName: array[TtkTokenKind] of string = (SYNS_AttrNull,
    SYNS_AttrAsmKey, SYNS_AttrAsm, SYNS_AttrAsmComment,
    SYNS_AttrSASMKey, SYNS_AttrSASM, SYNS_AttrSASMComment,
    SYNS_AttrRplKey, SYNS_AttrRpl, SYNS_AttrRplComment);

  DefaultAsmKeyWords: string = '!RPL'#13#10'ENDCODE'#13#10'{'#13#10'}'#13#10 +
  'GOTO'#13#10'GOSUB'#13#10'GOSBVL'#13#10'GOVLNG'#13#10'GOLONG'#13#10'SKIP' +
    #13#10'SKIPYES' + #13#10'->'#13#10'SKUB'#13#10'SKUBL'#13#10'SKC'#13#10'SKNC'#13#10'SKELSE' +
    #13#10'SKEC'#13#10'SKENC'#13#10'SKLSE'#13#10 + 'GOTOL'#13#10'GOSUBL'#13#10 +
    'RTN'#13#10'RTNC'#13#10'RTNNC'#13#10'RTNSC'#13#10'RTNCC'#13#10'RTNSXM'#13#10'RTI';
  OtherAsmKeyWords: array[0..5] of string = ('UP', 'EXIT', 'UPC', 'EXITC', 'UPNC', 'EXITNC');
  DefaultRplKeyWords: string =
    'CODEM'#13#10'ASSEMBLEM'#13#10'CODE'#13#10'ASSEMBLE'#13#10'IT'#13#10'ITE'#13#10'case'#13#10'::'#13#10';'#13#10'?SEMI'#13#10''''#13#10'#=case'#13#10'{'#13#10'}'#13#10'NAMELESS'#13#10'LOCAL'#13#10'LOCALNAME'#13#10'LABEL'#13#10 +
    'LOCALLABEL'#13#10'xNAME'#13#10'tNAME' + 'COLA'#13#10'NULLNAME'#13#10'xROMID'#13#10'#0=ITE'#13#10'#<ITE'#13#10'#=ITE'#13#10'#>ITE'#13#10'2''RCOLARPITE'#13#10'ANDITE'#13#10'COLAITE'#13#10'COLARPITE'#13#10'DUP#0=ITE'#13#10 +
    'EQITE'#13#10'ITE'#13#10'RPITE'#13#10'SysITE'#13#10'UNxSYMRPITE'#13#10'UserITE'#13#10'snnSYMRPITE'#13#10'snsSYMRPITE'#13#10'ssnSYMRPITE'#13#10'sssSYMRPITE'#13#10'$_EXIT'#13#10'DA1OK?NOTIT'#13#10'DA2aOK?NOTIT'#13#10 +
    'DA2bOK?NOTIT'#13#10'DA3OK?NOTIT'#13#10'DO#EXIT'#13#10'DO$EXIT'#13#10'DO%EXIT'#13#10'DOHXSEXIT'#13#10'DUP#0=IT'#13#10'EQIT'#13#10'GCDHEULPEXIT'#13#10'GSPLIT'#13#10'NOT_IT'#13#10'POINTEXIT'#13#10'POLYARIT'#13#10'RPIT'#13#10 +
    'parleftIT'#13#10'parrightIT'#13#10''''#13#10'IT'#13#10'ITE'#13#10'SEMI'#13#10'UNTIL'#13#10'LOOP'#13#10'?SEMI'#13#10'NOT?SEMI'#13#10'#0=case'#13#10'#1=case'#13#10'#<>case'#13#10'#<case'#13#10'#=case'#13#10'#=casedrop'#13#10 +
    '#=casedrpfls'#13#10'#>2case'#13#10'#>33case'#13#10'#>case'#13#10'%-1=case'#13#10'%0=case'#13#10'%1=case'#13#10'%2=case'#13#10'AEQ1stcase'#13#10'AEQopscase'#13#10'ANDNOTcase'#13#10'ANDcase'#13#10'C%-1=case'#13#10 +
    'C%0=case'#13#10'C%1=case'#13#10'C%2=case'#13#10'COLANOTcase'#13#10'COLAcase'#13#10'DUP#0=case'#13#10'EQUALNOTcase'#13#10'EQUALcase'#13#10'EQUALcasedrop'#13#10'EQUALcasedrp'#13#10'EQcase'#13#10'EQcaseDROP'#13#10 +
    'EQcasedrop'#13#10'EnvNGcase'#13#10'M-1stcasechs'#13#10'MEQ*case'#13#10'MEQ+case'#13#10'MEQ-case'#13#10'MEQ/case'#13#10'MEQ1stcase'#13#10'MEQCHScase'#13#10'MEQFCNcase'#13#10'MEQINVcase'#13#10'MEQSQcase'#13#10'MEQ^case'#13#10 +
    'MEQopscase'#13#10'Mid1stcase'#13#10'NOTBAKcase'#13#10'NOTLIBcase'#13#10'NOTLISTcase'#13#10'NOTMATRIXcase'#13#10'NOTROMPcase'#13#10'NOTSECOcase'#13#10'NOTTYPEcase'#13#10'NOTcase'#13#10'NOTcase2DROP'#13#10'NOTcase2drop'#13#10 +
    'NOTcaseDROP'#13#10'NOTcaseFALSE'#13#10'NOTcaseTRUE'#13#10'NOTcasedrop'#13#10'NULLargcase'#13#10'NcaseSIZEERR'#13#10'NcaseTYPEERR'#13#10'NoEdit?case'#13#10'ORcase'#13#10'OVER#=case'#13#10'REALcase'#13#10'REQcase'#13#10 +
    'REQcasedrop'#13#10'Z-1=case'#13#10'Z0=case'#13#10'Z1=case'#13#10'accNBAKcase'#13#10'accNLIBcase'#13#10'case'#13#10'case2DROP'#13#10'case2drop'#13#10'case2drpfls'#13#10'caseDEADKEY'#13#10'caseDROP'#13#10'caseDoBadKey'#13#10 +
    'caseDrpBadKy'#13#10'caseERRJMP'#13#10'caseFALSE'#13#10'caseSIZEERR'#13#10'caseTRUE'#13#10'casedrop'#13#10'casedrpfls'#13#10'casedrptru'#13#10'caseout'#13#10'cxcasecheck'#13#10'dARRYcase'#13#10'dIDNTNcase'#13#10'dLISTcase'#13#10 +
    'dMATRIXcase'#13#10'dREALNcase'#13#10'dREALcase'#13#10'dZINTNcase'#13#10'delimcase'#13#10'estcase'#13#10'idntcase'#13#10'idntlamcase'#13#10'j#-1=case'#13#10'j#0=case'#13#10'j#1=case'#13#10'j%-1=case'#13#10'j%0=case'#13#10 +
    'j%1=case'#13#10'jEQcase'#13#10'jZ-1=case'#13#10'jZ0=case'#13#10'jZ1=case'#13#10'namelscase'#13#10'need''case'#13#10'negrealcase'#13#10'ngsizecase'#13#10'nonopcase'#13#10'nonrmcase'#13#10'num#-1=case'#13#10'num#0=case'#13#10 +
    'num#1=case'#13#10'num-1=case'#13#10'num0=case'#13#10'num0case'#13#10'num1=case'#13#10'num2=case'#13#10'numb1stcase'#13#10'rebuildcase'#13#10'tok=casedrop'#13#10'wildcase'#13#10'zerdercase'#13#10;
  SasmNoField: string = 'LOOP'#13#10'RTNSXM'#13#10'RTN'#13#10'RTNSC'#13#10'RTNCC'#13#10'SETDEC'#13#10'SETHEX'#13#10'RSTK=C'#13#10'C=RSTK'#13#10'CLRST'#13#10'C=ST'#13#10'ST=C'#13#10'CSTEX'#13#10 +
  'RTI'#13#10'R0=A'#13#10'R1=A'#13#10'R2=A'#13#10'R3=A'#13#10'R4=A'#13#10'R0=C'#13#10'R1=C'#13#10'R2=C'#13#10'R3=C'#13#10'R4=C'#13#10'A=R0'#13#10'A=R1'#13#10'A=R2'#13#10'A=R3'#13#10'A=R4'#13#10 +
    'C=R0'#13#10'C=R1'#13#10'C=R2'#13#10'C=R3'#13#10'C=R4'#13#10'AR0EX'#13#10'AR1EX'#13#10'AR2EX'#13#10'AR3EX'#13#10'AR4EX'#13#10'CR0EX'#13#10'CR1EX'#13#10'CR2EX'#13#10'CR3EX'#13#10'CR4EX'#13#10 +
    'D0=A'#13#10'D0=C'#13#10'D1=A'#13#10'D1=C'#13#10'AD0EX'#13#10'AD1EX'#13#10'CD0EX'#13#10'CD1EX'#13#10'D0=AS'#13#10'D1=AS'#13#10'D0=CS'#13#10'D1=CD'#13#10'CD1XS'#13#10'CD0XS'#13#10'AD1XS'#13#10'AD0XS'#13#10 +
    'RTNC'#13#10'RTNNC'#13#10'OUT=CS'#13#10'OUT=C'#13#10'A=IN'#13#10'C=IN'#13#10'SHUTDN'#13#10'INTON'#13#10'C=ID'#13#10'CONFIG'#13#10'UNCNFG'#13#10'RSI'#13#10'PC=(A)'#13#10'PC=(C)'#13#10'INTOFF'#13#10 +
    'C+P+1'#13#10'RESET'#13#10'SREQ?'#13#10'ASLC'#13#10'BSLC'#13#10'CSLC'#13#10'DSLC'#13#10'ASRC'#13#10'BSRC'#13#10'CSRC'#13#10'DSRC'#13#10'ASRB'#13#10'BSRB'#13#10'CSRB'#13#10'DSRB'#13#10'PC=A'#13#10'PC=C'#13#10 +
    'A=PC'#13#10'C=PC'#13#10'APCEX'#13#10'CPCEX'#13#10'XM=0'#13#10'SB=0'#13#10'SR=0'#13#10'MP=0'#13#10'CLRHST'#13#10'?XM=0'#13#10'?SR=0'#13#10'?MP=0'#13#10'?SB=0'#13#10'RTNYES'#13#10'SKIPYES{'#13#10'{'#13#10'}'#13#10'UP'#13#10'EXIT'#13#10'EXITNC'#13#10'EXITC'#13#10'UPC'#13#10'UPNC' +
    '}SKELSE{'#13#10'SKC{'#13#10'SKNC{'#13#10'SKUB{'#13#10'SKUBL{'#13#10'SKIPC{'#13#10'SKIPNC{'#13#10'EXIT2'#13#10'EXIT3'#13#10'UP2'#13#10'UP3'#13#10'}SKLSE{'#13#10'}SKEC{'#13#10'}SKENC{'#13#10;

function StringCrc(S: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to length(s) do begin
    result := (result shr 4) xor (((result xor ord(s[i])) and $F) * $1081);
    result := (result shr 4) xor (((result xor (ord(s[i]) shr 4)) and $F) * $1081);
  end;
end;

{ TSpeedListObject }

constructor TSpeedListObject.create(name: string);
begin
  inherited create;
  FName := name;
end;

destructor TSpeedListObject.destroy;
begin
  if FSpeedList <> nil then
    FSpeedList.ObjectDeleted(Self);
  inherited destroy;
end;

procedure TSpeedListObject.SetName(const Value: string);
begin
  FName := Value;
  if FSpeedList <> nil then
    FSpeedList.NameChange(Self, Value);
end;

{ TSpeedStringList }

function TSpeedStringList.AddObj(const Value: TSpeedListObject): Integer;
var
  crc: integer;
  i: integer;
begin
  crc := StringCrc(Value.Name) mod High(Datas) + 1;
  if DatasUsed[crc] = lengthDatas[crc] then begin
    ReallocMem(datas[crc], (lengthDatas[crc] * 2 + 1) * SizeOf(datas[1][0]));
    lengthDatas[crc] := lengthDatas[crc] * 2 + 1;
  end;
  Datas[crc][DatasUsed[crc]] := Value;
  result := SumOfUsed[crc] + DatasUsed[crc];
  inc(DatasUsed[crc]);
  for i := crc + 1 to High(SumOfUsed) do
    inc(SumOfUsed[i]);
  Value.SpeedList := Self;
end;

function TSpeedStringList.Add(const Value: string): TSpeedListObject;
begin
  result := TSpeedListObject.Create(value);
  AddObj(Result);
end;

procedure TSpeedStringList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSpeedStringList.Clear;
var
  i, j: integer;
begin
  for i := low(datas) to high(datas) do begin
    for j := 0 to DatasUsed[i] - 1 do
      datas[i][j].free;
    datasUsed[i] := 0;
    ReallocMem(datas[i], 0);
    lengthDatas[i] := 0;
    SumOfUsed[i] := 0;
  end;
  Changed;
end;

constructor TSpeedStringList.create;
var
  i: integer;
begin
  inherited Create;
  for i := Low(Datas) to high(datas) do begin
    SumOfUsed[i] := 0;
    DatasUsed[i] := 0;
    lengthDatas[i] := 0;
    datas[i] := nil;
  end;
end;

destructor TSpeedStringList.Destroy;
begin
  Clear;
  inherited destroy;
end;

function TSpeedStringList.Find(const name: string): TSpeedListObject;
var
  crc: integer;
  i: integer;
begin
  crc := StringCrc(name) mod High(Datas) + 1;
  for i := 0 to DatasUsed[crc] - 1 do
    if Datas[crc][i].name = name then begin
      result := Datas[crc][i];
      exit;
    end;
  result := nil;
end;

function TSpeedStringList.Get(Index: Integer): string;
var
  i: integer;
begin
  for i := low(SumOfUsed) + 1 to High(SumOfUsed) do
    if Index > SumOfUsed[i] then begin
      result := Datas[i - 1][Index - SumOfUsed[i - 1]].name;
      exit;
    end;
  result := '';
end;

function TSpeedStringList.GetCount: integer;
begin
  result := SumOfUsed[High(datas)] + DatasUsed[High(Datas)];
end;

function TSpeedStringList.GetInObject(Index: Integer): TObject;
var
  i: integer;
begin
  for i := low(SumOfUsed) + 1 to High(SumOfUsed) do
    if Index > SumOfUSed[i] then begin
      result := Datas[i - 1][Index - SumOfUsed[i - 1]].pointer;
      exit;
    end;
  result := nil;
end;

function TSpeedStringList.GetObject(Index: Integer): TSpeedListObject;
var
  i: integer;
begin
  for i := low(SumOfUsed) + 1 to High(SumOfUsed) do
    if Index > SumOfUSed[i] then begin
      result := Datas[i - 1][Index - SumOfUsed[i - 1]];
      exit;
    end;
  result := nil;
end;

function TSpeedStringList.GetStringList: TStrings;
var
  i, j: integer;
begin
  result := TStringList.Create;
  for i := Low(Datas) to High(Datas) do
    for j := 0 to DatasUsed[i] - 1 do
      result.add(datas[i][j].name);
end;

function TSpeedStringList.GetText: string;
begin
  with StringList do begin
    result := Text;
    free;
  end;
end;

procedure TSpeedStringList.NameChange(const Obj: TSpeedListObject; const NewName: string);
var
  crc: integer;
  i: integer;
  j: integer;
begin
  crc := StringCrc(obj.Name) mod High(Datas) + 1;
  for i := 0 to DatasUsed[crc] - 1 do
    if Datas[crc][i] = Obj then begin
      for j := i + 1 to DatasUsed[crc] - 1 do
        Datas[i - 1] := Datas[i];
      for j := crc + 1 to High(Datas) do
        dec(SumOfUsed[j]);
      if DatasUsed[crc] < lengthDatas[crc] div 2 then begin
        ReallocMem(Datas[crc], DatasUsed[crc] * SizeOf(Datas[crc][0]));
        lengthDatas[crc] := DatasUsed[crc];
      end;
      AddObj(Obj);
      exit;
    end;
end;

procedure TSpeedStringList.ObjectDeleted(const obj: TSpeedListObject);
var
  crc: integer;
  i: integer;
  j: integer;
begin
  crc := StringCrc(obj.Name) mod High(Datas) + 1;
  for i := 0 to DatasUsed[crc] - 1 do
    if Datas[crc][i] = Obj then begin
      for j := i + 1 to DatasUsed[crc] - 1 do
        Datas[j - 1] := Datas[j];
      for j := crc + 1 to High(Datas) do
        dec(SumOfUsed[j]);
      Obj.FSpeedList := nil;
      exit;
    end;
end;

procedure TSpeedStringList.SetInObject(Index: Integer;
  const Value: TObject);
var
  i: integer;
begin
  for i := low(SumOfUsed) + 1 to High(SumOfUsed) do
    if Index > SumOfUSed[i] then begin
      Datas[i - 1][Index - SumOfUsed[i - 1]].pointer := value;
      exit;
    end;
end;

procedure TSpeedStringList.SetStringList(const value: TStrings);
var
  i: integer;
begin
  clear;
  for i := 0 to Value.Count - 1 do
    AddObj(TSpeedListObject.Create(value[i]));
end;

procedure TSpeedStringList.SetText(const Value: string);
var
  s: TStrings;
begin
  s := TStringList.Create;
  try
    s.Text := Value;
    StringList := s;
  finally
    s.Free;
  end;
end;

{ TSynHP48Syn }

constructor TSynHP48Syn.Create(AOwner: TComponent);
var
  i: TtkTokenKind;
  j, k: integer;
begin
  for i := low(TtkTokenKind) to High(TtkTokenKind) do
    Attribs[i] := TSynHighlighterAttributes.Create(tkTokenName[i]);
  inherited Create(AOwner);
  SetHighlightChange;
  FAsmKeyWords := TSpeedStringList.Create;
  FAsmKeyWords.Text := DefaultAsmKeyWords;
  for j := low(OtherAsmKeyWords) to High(OtherAsmKeyWords) do begin
    FAsmKeyWords.AddObj(TSpeedListObject.Create(OtherAsmKeyWords[j]));
    for k := 1 to 8 do
      FAsmKeyWords.AddObj(TSpeedListObject.Create(OtherAsmKeyWords[j] + IntToStr(k)));
  end;
  FRplKeyWords := TSpeedStringList.Create;
  FRplKeyWords.Text := DefaultRplKeyWords;
  FSAsmNoField := TSpeedStringList.Create;
  FSAsmNoField.Text := SAsmNoField;
  BaseRange := rsRpl;
  fRange := rsRpl;
  fDefaultFilter := SYNS_FilterHP48;
end; { Create }

destructor TSynHP48Syn.Destroy;
var
  i: TtkTokenKind;
begin
  for i := low(TtkTokenKind) to High(TtkTokenKind) do
    Attribs[i].Free;
  FAsmKeyWords.Free;
  FRplKeyWords.Free;
  FSAsmNoField.free;
  inherited Destroy;
end; { Destroy }

procedure TSynHP48Syn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 1;
  fEol := False;
  Next;
end; { SetLine }

function TSynHP48Syn.AsmComProc(c: char): TtkTokenKind;
begin
  Result := tkAsmComment;
  if (Run > Length(fLine)) then
    Result := NullProc
  else
    while Run <= Length(FLine) do
      if ((run = 1) or (fLine[run - 1] <= ' ')) and
        (fLine[Run] = '*') and
        ((run < Length(fLine)) and (fLine[run + 1] = c)) and
        ((run + 1 = Length(fLine)) or (fLine[run + 2] <= ' ')) then begin
        inc(run, 2);
        fRange := rsAsm;
        break;
      end
      else
        inc(Run);
end;

function TSynHP48Syn.RplComProc: TtkTokenKind;
begin
  Result := tkRplComment;
  if (Run > Length(fLine)) then
    Result := NullProc
  else
    while Run <= Length(FLine) do
      if fLine[Run] = ')' then begin
        inc(run);
        fRange := rsRpl;
        break;
      end
      else
        inc(Run);
end;

function TSynHP48Syn.SlashProc: TtkTokenKind;
begin
  if fRange = rsRpl then
    Result := IdentProc
  else if ((Run = 1) or (fLine[Run - 1] <= ' ')) and
    (fLine[Run] = '/') and
    (run < Length(fLine)) and
    (fLine[run + 1] = '*') and
    ((run + 1 = Length(fLine)) or (fLine[Run + 2] <= ' ')) then begin
    inc(Run, 2);
    Result := tkAsmComment;
    fRange := rsComAsm2;
  end
  else if (run < Length(fLine)) and (fLine[Run + 1] = '/') then begin
    inc(Run, 2);
    Result := tkAsmComment;
    while (run <= Length(fLine)) do
      if FLine[Run] in [#10, #13] then begin
        inc(Run);
        break;
      end
      else
        inc(Run);
  end
  else
    Result := IdentProc
end;

function TSynHP48Syn.ParOpenProc: TtkTokenKind;
begin
  if fRange = rsRpl then
    if ((Run = 1) and ((Length(fLine) = 1) or (fLine[Run + 1] <= ' '))) or
      ((fLine[Run - 1] <= ' ') and ((Length(fLine) = Run) or (fLine[Run + 1] <= ' '))) then begin
      inc(Run);
      Result := tkRplComment;
      fRange := rsComRpl;
    end
    else
      Result := IdentProc
  else if ((run = 1) or (fLine[run - 1] <= ' ')) and
    (fline[Run] = '(') and
    (run < Length(fLine)) and
    (fLine[run + 1] = '*') and
    ((run + 2 > Length(fLine)) or (fLine[run + 2] <= ' ')) then begin
    inc(Run, 2);
    Result := tkAsmComment;
    fRange := rsComAsm1;
  end
  else
    Result := IdentProc
end;

function TSynHP48Syn.PersentProc: TtkTokenKind;
begin
  if fRange = rsAsm then begin
    inc(Run);
    Result := tkAsmComment;
    while (run <= Length(fLine)) do
      case FLine[Run] of
        #10, #13: begin
            inc(Run);
            break;
          end;
      else
        inc(Run);
      end;
  end
  else
    Result := IdentProc;
end;

function TSynHP48Syn.StarProc: TtkTokenKind;
begin
  if fRange = rsRpl then begin
    inc(Run);
    Result := tkRplComment;
    while (run <= Length(fLine)) do
      case FLine[Run] of
        #10, #13: begin
            inc(Run);
            break;
          end;
      else
        inc(Run);
      end;
  end
  else
    Result := IdentProc;
end;

function TSynHP48Syn.IdentProc: TtkTokenKind;
var
  i: integer;
  s: string;
begin
  i := Run;
  EndOfToken;
  s := Copy(fLine, i, run - i);
  if fRange = rsAsm then
    if FAsmKeyWords.Find(s) <> nil then
      if (s = '!RPL') or (s = 'ENDCODE') then begin
        fRange := rsRpl;
        result := tkAsmKey;
      end
      else
        result := tkAsmKey
    else if fLine[i] <> '*' then
      result := tkAsm
    else
      result := tkAsmKey
  else if FRplKeyWords.Find(s) <> nil then
    if (s = 'CODEM') or (s = 'ASSEMBLEM') then begin
      fRange := rsAsm;
      result := tkAsmKey;
    end
    else if (s = 'CODE') or (s = 'ASSEMBLE') then begin
      fRange := rssAsm1;
      result := tksAsmKey;
    end
    else
      result := tkRplKey
  else
    result := tkRpl;
end;

function TSynHP48Syn.GetTokenFromRange: TtkTokenKind;
begin
  case frange of
    rsAsm: result := tkAsm;
    rssAsm1: result := tksAsmKey;
    rssAsm2: result := tksAsm;
    rssAsm3: result := tksAsmComment;
    rsRpl: result := tkRpl;
    rsComRpl: result := tkRplComment;
    rsComAsm1, rsComAsm2: result := tkAsmComment;
  else
    result := tkNull;
  end;
end;

function TSynHP48Syn.NullProc: TtkTokenKind;
begin
  Result := tkNull;
  fEol := True;
end;

function TSynHP48Syn.SpaceProc: TtkTokenKind;
begin
  inc(Run);
  while (Run <= Length(FLine)) and (FLine[Run] in [#1..#32]) do
    inc(Run);
  result := GetTokenFromRange;
end;

function TSynHP48Syn.Next1: TtkTokenKind;
begin
  fTokenPos := Run;
  if Run > Length(fLine) then
    result := NullProc
  else if fRange = rsComRpl then
    result := RplComProc
  else if fRange = rsComAsm1 then
    result := AsmComProc(')')
  else if fRange = rsComAsm2 then
    result := AsmComProc('/')
  else if frange = rssasm1 then
    result := SasmProc1
  else if frange = rssasm2 then
    result := sasmproc2
  else if frange = rssasm3 then
    result := sasmproc3
  else if fLine[Run] in [#1..#32] then
    result := SpaceProc
  else if fLine[Run] = '(' then
    result := ParOpenProc
  else if fLine[Run] = '%' then
    result := PersentProc
  else if fLine[Run] = '/' then
    result := SlashProc
  else if (run = 1) and (fRange = rsRpl) and (fLine[1] = '*') then
    result := StarProc
  else
    result := IdentProc;
end;

procedure TSynHP48Syn.Next2(tkk: TtkTokenKind);
begin
  fTockenKind := tkk;
end;

procedure TSynHP48Syn.Next;
begin
  Next2(Next1);
end;

function TSynHP48Syn.GetEol: Boolean;
begin
  Result := fEol;
end;

function TSynHP48Syn.GetToken: string;
var
  Len: LongInt;
  a: PChar;
begin
  a := @(fLine[fTokenPos]);
  Len := Run - fTokenPos;
  SetString(Result, a, Len);
end;

function TSynHP48Syn.GetTokenPos: Integer;
begin
  Result := fTokenPos - 1;
end;

function TSynHP48Syn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynHP48Syn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynHP48Syn.ResetRange;
begin
  fRange := BaseRange;
end;

function TSynHP48Syn.GetAttrib(Index: integer): TSynHighlighterAttributes;
begin
  Result := Attribs[TtkTokenKind(Index)];
end;

procedure TSynHP48Syn.SetAttrib(Index: integer; Value: TSynHighlighterAttributes);
begin
  Attribs[TtkTokenKind(Index)].Assign(Value);
end;

procedure TSynHP48Syn.EndOfToken;
begin
  while (Run <= Length(fLine)) and (FLine[Run] > ' ') do
    Inc(Run);
end;

{$IFNDEF SYN_CLX}
function TSynHP48Syn.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('AsmKeyWordList')
        then AsmKeywords.Text := r.ReadString('AsmKeyWordList');
      if r.ValueExists('RplKeyWordList')
        then RplKeywords.Text := r.ReadString('RplKeyWordList');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := false;
  finally r.Free;
  end;
end;

function TSynHP48Syn.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key, true) then begin
      Result := true;
      r.WriteString('AsmKeyWordList', AsmKeywords.Text);
      r.WriteString('RplKeyWordList', RplKeywords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else
      Result := false;
  finally r.Free;
  end;
end;
{$ENDIF}

procedure TSynHP48Syn.Assign(Source: TPersistent);
var
  i: TtkTokenKind;
begin
  if Source is TSynHP48Syn then begin
    for i := Low(Attribs) to High(Attribs) do begin
      Attribs[i].Background := TSynHP48Syn(source).Attribs[i].Background;
      Attribs[i].Foreground := TSynHP48Syn(source).Attribs[i].Foreground;
      Attribs[i].Style := TSynHP48Syn(source).Attribs[i].Style;
    end;
    AsmKeyWords.Text := TSynHP48Syn(source).AsmKeyWords.Text;
    RplKeyWords.Text := TSynHP48Syn(source).RplKeyWords.Text;
  end
  else
    inherited Assign(Source);
end;

function TSynHP48Syn.GetAttribCount: integer;
begin
  Result := Ord(High(Attribs)) - Ord(Low(Attribs)) + 1;
end;

function TSynHP48Syn.GetAttribute(idx: integer): TSynHighlighterAttributes;
begin // sorted by name
  if (idx <= Ord(High(TtkTokenKind))) then
    Result := Attribs[TtkTokenKind(idx)]
  else
    Result := nil;
end;

function TSynHP48Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterHP48;
end;

class function TSynHP48Syn.GetLanguageName: string;
begin
  Result := SYNS_LangHP48;
end;

procedure TSynHP48Syn.SetHighLightChange;
var
  i: TtkTokenKind;
begin
  for i := Low(Attribs) to High(Attribs) do begin
    Attribs[i].OnChange := DefHighLightChange;
    Attribs[i].InternalSaveDefaultValues;
  end;
end;

function TSynHP48Syn.SasmProc1: TtkTokenKind;
var
  i: integer;
  s: string;
begin
  Result := tksAsmKey;
  if run > Length(fLine) then
    exit;
  if FLine[Run] = '*' then begin
    frange := rssasm3;
    result := tksAsmComment;
    exit;
  end;
  if FLine[Run] >= ' ' then begin
    i := run;
    while (run <= Length(fLine)) and (FLine[run] > ' ') do
      inc(run);
    s := Copy(fLine, i, run - i);
    if (s = 'RPL') or (s = 'ENDCODE') then begin
      frange := rsRpl;
      exit;
    end;
  end;
  while (run <= Length(fLine)) and (FLine[run] <= ' ') and (FLine[run] <> #10) do
    inc(run);
  if run <= Length(fLine) then
    frange := rssasm2
  else
    frange := rssasm1;
end;

function TSynHP48Syn.SasmProc2: TtkTokenKind;
var
  i: integer;
  s: string;
begin
  Result := tksAsm;
  while (run <= Length(fLine)) and (FLine[run] <= ' ') and (fline[run] <> #10) do
    inc(run);
  if run > 30 then begin
    frange := rssasm3;
    exit;
  end;
  i := run;
  while (run <= Length(fLine)) and (FLine[run] > ' ') do
    inc(run);
  s := Copy(fLine, i, run - i);
  if (s = 'ENDCODE') or (s = 'RPL') then begin
    frange := rsRpl;
    result := tksAsmKey;
  end
  else begin
    if FSAsmNoField.Find(s) = nil then begin
      while (run <= Length(fLine)) and (FLine[run] <= ' ') and (FLine[run] <> #10) do
        inc(run);
      while (run <= Length(fLine)) and (FLine[run] > ' ') do
        inc(run);
      while (run <= Length(fLine)) and (FLine[run] <= ' ') and (FLine[run] <> #10) do
        inc(run);
    end;
    if run <= Length(fLine) then
      frange := rssasm3
    else
      frange := rssasm1;
  end;
end;

function TSynHP48Syn.SasmProc3: TtkTokenKind;
begin
  Result := tksAsmComment;
  while (run <= Length(fLine)) and (FLine[run] <> #10) do
    inc(run);
  if run <= Length(fLine) then inc(run);
  frange := rssasm1;
end;

function TSynHP48Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := GetAttrib(Ord(fTockenKind));
end;

function TSynHP48Syn.GetTokenKind: integer;
begin
  Result := Ord(fTockenKind);
end;

function TSynHP48Syn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

{$IFNDEF SYN_CPPB_1}
initialization
  RegisterPlaceableHighlighter(TSynHP48Syn);
{$ENDIF}
end.
