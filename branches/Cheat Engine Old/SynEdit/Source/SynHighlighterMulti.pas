{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterMulti.pas, released 2000-06-23.
The Original Code is based on mwMultiSyn.pas by Willo van der Merwe, part of the
mwEdit component suite.

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

$Id: SynHighlighterMulti.pas,v 1.34 2004/07/09 13:03:55 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Multiple-highlighter syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999, converted to SynEdit 2000-06-23)
@lastmod(2000-06-23)
The SynHighlighterMulti unit provides SynEdit with a multiple-highlighter syntax highlighter.
This highlighter can be used to highlight text in which several languages are present, such as HTML.
For example, in HTML as well as HTML tags there can also be JavaScript and/or VBScript present.
}

{$IFNDEF QSYNHIGHLIGHTERMULTI}
unit SynHighlighterMulti;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  Classes;

type
  TOnCheckMarker = procedure (Sender: TObject; var StartPos, MarkerLen: Integer;
    var MarkerText: String; Line: Integer) of object;

  TScheme = class(TCollectionItem)
  private
    fEndExpr: string;
    fStartExpr: string;
    fHighlighter: TSynCustomHighLighter;
    fMarkerAttri: TSynHighlighterAttributes;
    fSchemeName: TComponentName;
    fCaseSensitive: Boolean;
    fOnCheckStartMarker: TOnCheckMarker;
    fOnCheckEndMarker: TOnCheckMarker;
    function ConvertExpression(const Value: string): string;
    procedure MarkerAttriChanged(Sender: TObject);
    procedure SetMarkerAttri(const Value: TSynHighlighterAttributes);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetEndExpr(const Value: string);
    procedure SetStartExpr(const Value: string);
    procedure SetCaseSensitive(const Value: Boolean);

  protected
{$IFDEF SYN_COMPILER_3_UP}
    function GetDisplayName: String; override;
    procedure SetDisplayName(const Value: String); override;
{$ENDIF}
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive
      default True;
    property StartExpr: string read fStartExpr write SetStartExpr;
    property EndExpr: string read fEndExpr write SetEndExpr;
    property Highlighter: TSynCustomHighlighter read fHighlighter
      write SetHighlighter;
    property MarkerAttri: TSynHighlighterAttributes read fMarkerAttri
      write SetMarkerAttri;
    property SchemeName: TComponentName read fSchemeName write fSchemeName;
    property OnCheckStartMarker: TOnCheckMarker read fOnCheckStartMarker write fOnCheckStartMarker;
    property OnCheckEndMarker: TOnCheckMarker read fOnCheckEndMarker write fOnCheckEndMarker;
  end;

  TgmSchemeClass = class of TScheme;

  TSynMultiSyn = class;

  TSchemes = class(TCollection)
  private
    fOwner: TSynMultiSyn;
    function GetItems(Index: integer): TScheme;
    procedure SetItems(Index: integer; const Value: TScheme);
{$IFDEF SYN_COMPILER_3_UP}
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
{$ENDIF}
  public
    constructor Create(aOwner: TSynMultiSyn);
    property Items[aIndex: integer]: TScheme read GetItems write SetItems;
      default;
  end;

  TMarker = class
  protected
    fScheme: integer;
    fStartPos: integer;
    fMarkerLen: integer;
    fMarkerText: string;
    fIsOpenMarker: boolean;
  public
    constructor Create(aScheme, aStartPos, aMarkerLen: integer;
      aIsOpenMarker: boolean; const aMarkerText: string);
  end;


  TRangeOperation = (roGet, roSet);

  TRangeProc = procedure (Operation: TRangeOperation; var Range: cardinal) of object;

  TCustomRangeEvent = procedure (Sender: TSynMultiSyn; Operation: TRangeOperation;
    var Range: pointer) of object;

  {
  * Usage notes *
    If you don't need to nest MultiSyns as Schemes, just as DefaultHighlighter,
  you can nest up to 2 MultiSyns, each of them containing up to 7 Schemes. This
  is the way MultiSyn works best. (implemented in NewRangeProc)
    If you need to use a MultiSyn nested as Scheme, then you can nest up to
  5 MultiSyns, but Ranges aren't persisted across occurrences of Schemes that
  have multiple lines. (implemented in OldRangeProc)
    Clarification: when I say "you can nest up to X" MultiSyns, I mean having
  X+1 levels of MultiSyns.

  MultiSyn doesn't work by default with dynamic highlighters; you must use
  OnCustomRange. This is because dynamic highlighters' Ranges are pointers,
  but MultiSyn needs Ranges to be ordinal values smaller than 16 (4 bits).

  OnCustomRange:
    When Operation is roGet, user should store in the 'Range' parameter the
    information to allow restoring the current state of the highlighter.
    When Operation is roSet, user should restore highlighter state (CurrScheme,
    DefaultHighlighter.Range and, if the case, Schemes[CurrScheme].Range)
    according to 'Range' value.
  CurrScheme:
    Index of the scheme that is currently parsing. DefaultHighlighter maps to -1.

  * Implementation notes *
  fTmpLine:
    The (partial) line that the current scheme is parsing. It's necessary
    because SetLine takes a string as parameter, but most highlighters
    use a PChar to point to it, breaking reference count.
    When the editor calls 'Next' so the highlighter returns the next token,
    the string may have gone or may have changed.
  fTmpRange:
    Using the OldRangeProc, fTmpRange was the only way to restore the Range
    of the DefaultHighlighter after a Scheme spanned across multiple lines.
    With the NewRangeProc, the only use for it is restoring DefaultHighLighter's
    Range in case a nested MultiSyn uses the highlighter too.
  }

  TSynMultiSyn = class(TSynCustomHighLighter)
  private
    fRangeProc: TRangeProc;
    fDefaultLanguageName: String;
    fMarkers: TList;
    fMarker: TMarker;
    fNextMarker: integer;
    fCurrScheme: integer;
    fTmpLine: String;
    fTmpRange: pointer;
    fOnCustomRange: TCustomRangeEvent;
    procedure SetDefaultHighlighter(const Value: TSynCustomHighLighter);
    function GetMarkers(aIndex: integer): TMarker;
    property Markers[aIndex: integer]: TMarker read GetMarkers;
    procedure DoCheckMarker(Scheme:TScheme; StartPos, MarkerLen: Integer;
      const MarkerText: String; Start: Boolean; Line: Integer);
    procedure SetOnCustomRange(const Value: TCustomRangeEvent);
  protected
    fSchemes: TSchemes;
    fDefaultHighlighter: TSynCustomHighLighter;
    fLine: string;
    fLineNumber: Integer;
    fTokenPos: integer;
    fRun: Integer;
    fSampleSource: string;
    procedure Loaded; override;
    procedure SetSchemes(const Value: TSchemes);
    procedure ClearMarkers;
    function GetIdentChars: TSynIdentChars; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetAttribCount: integer; override;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; override;
    procedure HookHighlighter(aHL: TSynCustomHighlighter);
    procedure UnhookHighlighter(aHL: TSynCustomHighlighter);
    procedure Notification(aComp: TComponent; aOp: TOperation); override;
    function GetSampleSource: string; override;
    procedure SetSampleSource(Value: string); override;
    //
    procedure OldRangeProc(Operation: TRangeOperation; var Range: cardinal);
    procedure NewRangeProc(Operation: TRangeOperation; var Range: cardinal);
    procedure UserRangeProc(Operation: TRangeOperation; var Range: cardinal);
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UpdateRangeProcs: boolean;
    property CurrScheme: integer read fCurrScheme write fCurrScheme;
    property CurrLine: string read fLine;
{$IFNDEF SYN_CLX}
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; override;
{$ENDIF}
  published
    property Schemes: TSchemes read fSchemes write SetSchemes;
    property DefaultHighlighter: TSynCustomHighLighter read fDefaultHighlighter
      write SetDefaultHighlighter;
    property DefaultLanguageName: String read fDefaultLanguageName
      write fDefaultLanguageName;
    property OnCustomRange: TCustomRangeEvent read fOnCustomRange write SetOnCustomRange;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditMiscProcs,
  QSynRegExpr,
  QSynEditStrConst,
{$ELSE}
  Graphics,
  SynEditMiscProcs,
  SynRegExpr,
  SynEditStrConst,
{$ENDIF}
  SysUtils;

procedure CheckExpression(const aExpr: string);
var
  iParser: TRegExpr;
begin
  iParser := TRegExpr.Create;
  try
    iParser.Expression := aExpr;
    try
      iParser.Compile;
    except
      on E: ERegExpr do
      begin
        if E.ErrorCode < 1000 then
          E.Message := Format( '"%s" is not a valid Regular Expression.'#13'Error (pos %d): %s',
            [ aExpr, E.CompilerErrorPos, Copy( iParser.ErrorMsg(E.ErrorCode), 16, MaxInt) ] );
        raise;
      end;
    end;
  finally
    iParser.Free;
  end;
end;

{ TMarker }

constructor TMarker.Create(aScheme, aStartPos,
  aMarkerLen: integer; aIsOpenMarker: boolean; const aMarkerText: string);
begin
  fScheme := aScheme;
  fStartPos := aStartPos;
  fMarkerLen := aMarkerLen;
  fIsOpenMarker := aIsOpenMarker;
  fMarkerText := aMarkerText;
end;

{ TSynMultiSyn }

procedure TSynMultiSyn.ClearMarkers;
var
  i: integer;
begin
  for i := 0 to fMarkers.Count - 1 do
    TObject(fMarkers[i]).Free;
  fMarkers.Clear;
end;

constructor TSynMultiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSchemes := TSchemes.Create(Self);
  fCurrScheme := -1;
  fMarkers := TList.Create;
  fRangeProc := NewRangeProc;
end;

destructor TSynMultiSyn.Destroy;
begin
  ClearMarkers;
  { unhook notification handlers }
  Schemes.Clear;
  DefaultHighlighter := nil;
  inherited Destroy;
  fSchemes.Free;
  fMarkers.Free;
end;

function TSynMultiSyn.GetAttribCount: integer;
var
  cScheme: integer;
begin
  Result := Schemes.Count;
  if DefaultHighlighter <> nil then
    Inc( Result, DefaultHighlighter.AttrCount );
  for cScheme := 0 to Schemes.Count -1 do
    if Schemes[cScheme].Highlighter <> nil then
      Inc( Result, Schemes[cScheme].Highlighter.AttrCount );
end;

function TSynMultiSyn.GetAttribute(
  idx: integer): TSynHighlighterAttributes;
var
  cScheme: integer;
  iHL: TSynCustomHighlighter;
begin
  if idx < Schemes.Count then
    Result := Schemes[ idx ].MarkerAttri
  else begin
    Dec( idx, Schemes.Count );
    if DefaultHighlighter <> nil then
      if idx < DefaultHighlighter.AttrCount then
      begin
        Result := DefaultHighlighter.Attribute[idx];
        Exit;
      end
      else
        Dec( idx, DefaultHighlighter.AttrCount );
    for cScheme := 0 to Schemes.Count -1 do
    begin
      iHL := Schemes[cScheme].Highlighter;
      if iHL <> nil then
        if idx < iHL.AttrCount then
        begin
          Result := iHL.Attribute[idx];
          Exit;
        end
        else
          Dec( idx, iHL.AttrCount );
    end;
    Result := nil;
  end;
end;

function TSynMultiSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
var
  iHL: TSynCustomHighlighter;
begin
  if (CurrScheme >= 0) and (Schemes[CurrScheme].Highlighter <> nil) then
    iHL := Schemes[CurrScheme].Highlighter
  else
    iHL := DefaultHighlighter;
  { the typecast to TSynMultiSyn is only necessary because the
  GetDefaultAttribute method is protected.
  And don't worry: this really works }
  if iHL <> nil then begin
    Result := TSynMultiSyn(iHL).GetDefaultAttribute(Index)
  end else
    Result := nil;
end;

function TSynMultiSyn.GetEol: Boolean;
begin
  if fMarker <> nil then
    Result := False
  else if fCurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.GetEol
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetEol
  else
    Result := fRun > Length(fLine) + 2;
end;

function TSynMultiSyn.GetIdentChars: TSynIdentChars;
begin
  if CurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.IdentChars
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.IdentChars
  else
    Result := inherited GetIdentChars;
end;

class function TSynMultiSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneralMulti;
end;

function TSynMultiSyn.GetMarkers(aIndex: integer): TMarker;
begin
  Result := TMarker( fMarkers[ aIndex ] );
end;

procedure TSynMultiSyn.OldRangeProc(Operation: TRangeOperation; var Range: cardinal);
const
  MaxNestedMultiSyn = 6;
  { number of bits of the Range that will be used to store the SchemeIndex }
  SchemeIndexSize = 4;
  MaxSchemeCount = (1 shl SchemeIndexSize) -1;
  { number of bits of the Range that will be used to store the SchemeRange }
  SchemeRangeSize = 8;
  MaxSchemeRange = (1 shl SchemeRangeSize) -1;
var
  iHL: TSynCustomHighlighter;
  iSchemeIndex: cardinal;
  iSchemeRange: cardinal;
begin
  if Operation = roGet then
  begin
    if (fCurrScheme < 0) then
      iHL := DefaultHighlighter
    else
      iHL := Schemes[fCurrScheme].Highlighter;
    iSchemeIndex := fCurrScheme +2;
    Assert( iSchemeIndex <= MaxSchemeCount );
    if iHL <> nil then begin
      iSchemeRange := cardinal( iHL.GetRange );
      Assert( (iSchemeRange <= MaxSchemeRange) or (iHL is TSynMultiSyn) );
    end else
      iSchemeRange := 0;
    { checks the limit of nested MultiSyns }
    Assert( iSchemeRange shr ((MaxNestedMultiSyn -1)*SchemeIndexSize + SchemeRangeSize) = 0 );
    iSchemeRange := (iSchemeRange shl SchemeIndexSize) or iSchemeIndex;
    Range := iSchemeRange;
  end
  else begin
    if Range = 0 then
      Exit;
    iSchemeRange := cardinal(Range);
    fCurrScheme := integer(iSchemeRange and MaxSchemeCount) -2;
    iSchemeRange := iSchemeRange shr SchemeIndexSize;
    if (CurrScheme < 0) then begin
      if DefaultHighlighter <> nil then
        DefaultHighlighter.SetRange( pointer(iSchemeRange) );
    end else begin
      Schemes[CurrScheme].Highlighter.SetRange( pointer(iSchemeRange) );
    end;
  end;
end;

function TSynMultiSyn.GetToken: string;
begin
  if DefaultHighlighter = nil then
    Result := fLine
  else
    Result := Copy( fLine, fTokenPos +1, fRun - fTokenPos -1)
end;

function TSynMultiSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if fMarker <> nil then
    Result := Schemes[fMarker.fScheme].MarkerAttri
  else if CurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.GetTokenAttribute
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenAttribute
  else
    Result := nil;
end;

function TSynMultiSyn.GetTokenKind: integer;
begin
  if fMarker <> nil then
    Result := 0
  else if fCurrScheme >= 0 then
    Result := Schemes[fCurrScheme].Highlighter.GetTokenKind
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenKind
  else
    Result := 0;
end;

function TSynMultiSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynMultiSyn.HookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.FreeNotification( Self );
  aHL.HookAttrChangeEvent( DefHighlightChange );
end;

procedure TSynMultiSyn.Next;
var
  iToken: String;
  iHL: TSynCustomHighlighter;
begin
  if DefaultHighlighter = nil then begin
    if fRun > 1 then
      Inc( fRun )
    else
      fRun := Length(fLine) + 2;
    Exit;
  end;

  if (fNextMarker < fMarkers.Count) and (fRun >= Markers[fNextMarker].fStartPos) then begin
    fMarker := Markers[ fNextMarker ];
    if fMarker.fIsOpenMarker then
    begin
      fCurrScheme := fMarker.fScheme;
      fTmpRange := DefaultHighlighter.GetRange;
      Schemes[CurrScheme].Highlighter.ResetRange;
    end;
    Inc( fNextMarker );
    fTokenPos := fRun -1;
    Inc( fRun, fMarker.fMarkerLen );
    Exit;
  end;

  if (fRun = 1) then begin
    if fMarkers.Count = 0 then
      fTmpLine := fLine
    else
      fTmpLine := Copy( fLine, 1, Markers[fNextMarker].fStartPos -1 );
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.SetLine( fTmpLine, fLineNumber );
  end else if fMarker <> nil then begin
    if not fMarker.fIsOpenMarker then
    begin
      fCurrScheme := -1;
      DefaultHighlighter.SetRange( fTmpRange );
    end;
    fMarker := nil;
    {}
    if fNextMarker < fMarkers.Count then
      fTmpLine := Copy( fLine, fRun, Markers[fNextMarker].fStartPos - fRun  )
    else
      fTmpLine := Copy( fLine, fRun, MaxInt );
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.SetLine( fTmpLine, fLineNumber );
  end else begin
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.Next;
  end;

  fTokenPos := iHL.GetTokenPos;
  iToken := iHL.GetToken;
  if fNextMarker > 0 then begin
    with Markers[ fNextMarker -1 ] do
      Inc( fTokenPos, fStartPos + fMarkerLen -1 );
  end;
  Inc( fRun, (fTokenPos - fRun +1) + Length(iToken) );
end;

procedure TSynMultiSyn.Notification(aComp: TComponent; aOp: TOperation);
var
  cScheme: integer;
begin
  inherited;
  // 'opRemove' doesn't mean the component is being destroyed. It means it's
  // being removed from its Owner's list of Components.
  if (aOp = opRemove) and (aComp is TSynCustomHighlighter) and
    (csDestroying in aComp.ComponentState) then
  begin
    if DefaultHighlighter = aComp then
      DefaultHighlighter := nil;
    for cScheme := 0 to Schemes.Count -1 do
      if Schemes[ cScheme ].Highlighter = aComp then
        Schemes[ cScheme ].Highlighter := nil;
  end;
end;

procedure TSynMultiSyn.ResetRange;
begin
  fCurrScheme := -1;
  if DefaultHighlighter <> nil then
  begin
    DefaultHighlighter.ResetRange;
    fTmpRange := DefaultHighlighter.GetRange;
  end;
end;

procedure TSynMultiSyn.SetDefaultHighlighter(
  const Value: TSynCustomHighLighter);
const
  sDefaultHlSetToSelf = 'A SynMultiSyn cannot be its own DefaultHighlighter.';
begin
  if DefaultHighlighter <> Value then begin
    if Value = Self then
      raise Exception.Create( sDefaultHlSetToSelf );
    if DefaultHighlighter <> nil then
      UnhookHighlighter( DefaultHighlighter );
    fDefaultHighlighter := Value;
    if DefaultHighlighter <> nil then
      HookHighlighter( DefaultHighlighter );
    DefHighlightChange( Self );
  end;
end;

procedure TSynMultiSyn.DoCheckMarker(Scheme:TScheme; StartPos, MarkerLen: Integer;
  const MarkerText: String; Start: Boolean; Line: Integer);
var
  aStartPos: Integer;
  aMarkerLen: Integer;
  aMarkerText: String;
begin
  aStartPos:=StartPos;
  aMarkerLen:=MarkerLen;
  aMarkerText:=MarkerText;
  if (Start) and Assigned(Scheme.OnCheckStartMarker) then
    Scheme.OnCheckStartMarker(Self,aStartPos,aMarkerLen,aMarkerText,Line)
  else if (not Start) and Assigned(Scheme.OnCheckEndMarker) then
    Scheme.OnCheckEndMarker(Self,aStartPos,aMarkerLen,aMarkerText,Line);
  if (aMarkerText<>'') and (aMarkerLen>0) then
    begin
    fMarkers.Add(TMarker.Create(Scheme.Index, aStartPos, aMarkerLen,Start,aMarkerText));
    end;
end;

procedure TSynMultiSyn.SetLine(NewValue: string; LineNumber: Integer);
var
  iParser: TRegExpr;
  iScheme: TScheme;
  iExpr: String;
  iLine: String;
  iEaten: integer;
  cScheme: integer;
begin
  ClearMarkers;

  iParser := TRegExpr.Create;
  try
    iEaten := 0;
    iLine := NewValue;
    if CurrScheme >= 0
    then
      iScheme := fSchemes[ CurrScheme ]
    else
      iScheme := nil;
    while iLine <> '' do
      if iScheme <> nil then begin
        iParser.Expression := iScheme.EndExpr;
        iParser.ModifierI := not iScheme.CaseSensitive;
        if iParser.Exec( iLine ) then begin
          iExpr := Copy( NewValue, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0] );
          DoCheckMarker(iScheme, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0],iExpr,False, LineNumber);
          Delete( iLine, 1, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
          Inc( iEaten, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
          iScheme := nil;
        end else
          break;
      end else begin
        for cScheme := 0 to Schemes.Count -1 do begin
          iScheme := Schemes[ cScheme ];
          if (iScheme.StartExpr = '') or (iScheme.EndExpr = '') or
            (iScheme.Highlighter = nil) or (not iScheme.Highlighter.Enabled) then
          begin
            continue;
          end;
          iParser.Expression := iScheme.StartExpr;
          iParser.ModifierI := not iScheme.CaseSensitive;
          if iParser.Exec( iLine ) then begin
            iExpr := Copy( NewValue, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0] );
            DoCheckMarker(iScheme, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0],iExpr,True, LineNumber);
            Delete( iLine, 1, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
            Inc( iEaten, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
            break;
          end;
        end; {for}
        if cScheme >= Schemes.Count then
          break;
      end; {else}

  finally
    iParser.Free;
  end;

  fLineNumber := LineNumber;
  fLine := NewValue;
  fMarker := nil;
  fRun := 1;
  fTokenPos := 0;
  fNextMarker := 0;
  Next;
end;

procedure TSynMultiSyn.SetSchemes(const Value: TSchemes);
begin
  fSchemes.Assign(Value);
end;

procedure TSynMultiSyn.UnhookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.UnhookAttrChangeEvent( DefHighlightChange );
{$IFDEF SYN_COMPILER_5_UP}
  aHL.RemoveFreeNotification( Self );
{$ENDIF}
end;

function TSynMultiSyn.GetSampleSource: string;
begin
  Result := fSampleSource;
end;

procedure TSynMultiSyn.SetSampleSource(Value: string);
begin
  fSampleSource := Value;
end;

{$IFNDEF SYN_CLX}
function TSynMultiSyn.LoadFromRegistry(RootKey: HKEY;
  Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.LoadFromRegistry( RootKey, Key + '\DefaultHighlighter' )
  else
    Result := False;
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    for i := 0 to Schemes.Count-1 do
      if (Schemes[i].SchemeName <> '') and
        r.OpenKeyReadOnly(Key + '\' + Schemes[i].SchemeName) then
      begin
        Result := Schemes[i].MarkerAttri.LoadFromRegistry(r) and Result;
        r.CloseKey;
        Result := (Schemes[i].Highlighter <> nil) and
          Schemes[i].Highlighter.LoadFromRegistry( RootKey,
          Key + '\' + Schemes[i].SchemeName ) and Result;
      end
      else
        Result := False;
  finally
    r.Free;
  end;
end;

function TSynMultiSyn.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.SaveToRegistry( RootKey, Key + '\DefaultHighlighter' )
  else
    Result := False;
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    for i := 0 to Schemes.Count-1 do
      if (Schemes[i].SchemeName <> '') and
        r.OpenKey(Key + '\' + Schemes[i].SchemeName, True) then
      begin
        Result := Schemes[i].MarkerAttri.SaveToRegistry(r) and Result;
        r.CloseKey;
        Result := (Schemes[i].Highlighter <> nil) and
          Schemes[i].Highlighter.SaveToRegistry( RootKey,
          Key + '\' + Schemes[i].SchemeName ) and Result;
      end
      else
        Result := False;
  finally
    r.Free;
  end;
end;
{$ENDIF}

function TSynMultiSyn.GetRange: Pointer;
begin
  Result := nil;
  fRangeProc( roGet, cardinal(Result) );
end;

procedure TSynMultiSyn.SetRange(Value: Pointer);
begin
  fRangeProc( roSet, cardinal(Value) );
end;

procedure TSynMultiSyn.NewRangeProc(Operation: TRangeOperation; var Range: cardinal);
const
  SchemeIndexSize = 3;
  MaxSchemeCount = (1 shl SchemeIndexSize) -1;
  SchemeRangeSize = 4;
  MaxSchemeRange = (1 shl SchemeRangeSize) -1;
begin
  if Operation = roGet then
  begin
    if DefaultHighlighter <> nil then
      Range := cardinal( DefaultHighlighter.GetRange )
    else
      Range := 0;
    if CurrScheme >= 0 then
    begin
      Assert( cardinal( Schemes[CurrScheme].Highlighter.GetRange ) <= MaxSchemeRange );
      Range := Range shl SchemeRangeSize;
      Range := Range or cardinal( Schemes[CurrScheme].Highlighter.GetRange );
    end;
    Assert( CurrScheme <= MaxSchemeCount );
    Range := Range shl SchemeIndexSize;
    Range := Range or cardinal(CurrScheme + 1);
  end
  else begin
    CurrScheme := integer(Range and MaxSchemeCount) -1;
    Range := Range shr SchemeIndexSize;
    if CurrScheme >= 0 then
    begin
      Schemes[CurrScheme].Highlighter.SetRange( pointer(Range and MaxSchemeRange) );
      Range := Range shr SchemeRangeSize;
    end;
    if DefaultHighlighter <> nil then
    begin
      fTmpRange := pointer(Range);
      DefaultHighlighter.SetRange( fTmpRange );
    end;
  end;
end;

function TSynMultiSyn.UpdateRangeProcs: boolean;
// determines the appropriate RangeProcs and returns whether they were changed
var
  i: integer;
  OldProc: TRangeProc;
begin
  OldProc := fRangeProc;
  if Assigned( OnCustomRange ) then
    fRangeProc := UserRangeProc
  else begin
    fRangeProc := NewRangeProc;
    for i := 0 to Schemes.Count -1 do
      if Schemes[i].Highlighter is TSynMultiSyn then
      begin
        fRangeProc := OldRangeProc;
        break;
      end;
  end;
  Result := TMethod(OldProc).Code <> TMethod(fRangeProc).Code;
  if Result then
    DefHighlightChange( Self );
end;

procedure TSynMultiSyn.UserRangeProc(Operation: TRangeOperation; var Range: cardinal);
begin
  OnCustomRange( Self, Operation, pointer(Range) );
  if (Operation = roSet) and (DefaultHighlighter <> nil) then
    fTmpRange := DefaultHighlighter.GetRange;
end;

procedure TSynMultiSyn.SetOnCustomRange(const Value: TCustomRangeEvent);
begin
  if ( TMethod(OnCustomRange).Code <> TMethod(Value).Code ) or
    ( TMethod(OnCustomRange).Data <> TMethod(Value).Data ) then
  begin
    fOnCustomRange := Value;
    UpdateRangeProcs;
  end;
end;

procedure TSynMultiSyn.Loaded;
begin
  inherited;
  DefHighlightChange( Self );
end;

{ TSchemes }

constructor TSchemes.Create(aOwner: TSynMultiSyn);
begin
  inherited Create(TScheme);
  fOwner := aOwner;
end;

function TSchemes.GetItems(Index: integer): TScheme;
begin
  Result := inherited Items[Index] as TScheme;
end;

{$IFDEF SYN_COMPILER_3_UP}
function TSchemes.GetOwner: TPersistent;
begin
  Result := fOwner;
end;
{$ENDIF}

procedure TSchemes.SetItems(Index: integer; const Value: TScheme);
begin
  inherited Items[Index] := Value;
end;

{$IFDEF SYN_COMPILER_3_UP}
procedure TSchemes.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    fOwner.DefHighlightChange( Item )
  else // pass the MultiSyn as the Sender so Editors reparse their text
    fOwner.DefHighlightChange( fOwner );
end;
{$ENDIF}

{ TScheme }

function TScheme.ConvertExpression(const Value: String): String;
begin
  if not CaseSensitive then
    Result := AnsiUpperCase(Value)
  else
    Result := Value;
end;

constructor TScheme.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fCaseSensitive := True;
  fMarkerAttri := TSynHighlighterAttributes.Create(SYNS_AttrMarker);
  fMarkerAttri.OnChange := MarkerAttriChanged;
  MarkerAttri.Background := clYellow;
  MarkerAttri.Style := [fsBold];
  MarkerAttri.InternalSaveDefaultValues;
end;

destructor TScheme.Destroy;
begin
  { unhook notification handlers }
  Highlighter := nil;
  inherited Destroy;
  fMarkerAttri.Free;
end;

{$IFDEF SYN_COMPILER_3_UP}
function TScheme.GetDisplayName: String;
begin
  if SchemeName <> '' then
    Result := SchemeName
  else
    Result := inherited GetDisplayName;
end;
{$ENDIF SYN_COMPILER_3_UP}

procedure TScheme.MarkerAttriChanged(Sender: TObject);
begin
  Changed( False );
end;

procedure TScheme.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    fCaseSensitive := Value;
    Changed( True );
  end;
end;

{$IFDEF SYN_COMPILER_3_UP}
procedure TScheme.SetDisplayName(const Value: String);
begin
  SchemeName := Value;
end;
{$ENDIF SYN_COMPILER_3_UP}

procedure TScheme.SetEndExpr(const Value: string);
var
  OldValue: String;
begin
  if fEndExpr <> Value then
  begin
    if Value <> '' then
      CheckExpression( Value );
    OldValue := fEndExpr;
    fEndExpr := Value;
    if ConvertExpression( OldValue ) <> ConvertExpression( Value ) then
      Changed( True );
  end;
end;

procedure TScheme.SetHighlighter(const Value: TSynCustomHighLighter);
var
  iOwner: TSynMultiSyn;
  iAlreadyRepainted: boolean;
begin
  if Highlighter <> Value then
  begin
    iOwner := TSchemes(Collection).fOwner;
    if (Highlighter <> nil) and (Highlighter <> iOwner) then
      iOwner.UnhookHighlighter( Highlighter );
    fHighlighter := Value;
    if (Highlighter <> nil) and (Highlighter <> iOwner) then
      iOwner.HookHighlighter( Highlighter );
    if Highlighter is TSynMultiSyn then
      iAlreadyRepainted := iOwner.UpdateRangeProcs
    else
      iAlreadyRepainted := False;
    if not iAlreadyRepainted then
      Changed( True );
  end;
end;

procedure TScheme.SetMarkerAttri(const Value: TSynHighlighterAttributes);
begin
  fMarkerAttri.Assign(Value);
end;

procedure TScheme.SetStartExpr(const Value: string);
var
  OldValue: String;
begin
  if fStartExpr <> Value then
  begin
    if Value <> '' then
      CheckExpression( Value );
    OldValue   := fStartExpr;
    fStartExpr := Value;
    if ConvertExpression( Value ) <> ConvertExpression( OldValue ) then
      Changed( True );
  end;
end;

end.