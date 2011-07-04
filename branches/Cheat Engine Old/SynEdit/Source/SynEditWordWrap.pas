{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynEditWordWrap.pas by Flávio Etrusco, released 2003-12-11.
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

$Id: SynEditWordWrap.pas,v 1.11 2007/01/24 00:17:33 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
//todo: Use a single implementation of ReWrapLines that takes starting line and number of lines to rewrap
//todo: Tweak code to try finding better wrapping points. Some support by the highlighters will be needed, probably.
//todo: Document the code
//todo: The length of the last Row of a Line could be calculated from the Line length instead of being stored. This would be only useful when most of the lines aren't wrapped.

{$IFNDEF QSYNEDITWORDWRAP}
unit SynEditWordWrap;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
  QSynEditTextBuffer,
  QSynEdit,
{$ELSE}
  SynEditTypes,
  SynEditTextBuffer,
  SynEdit,
{$ENDIF}
  SysUtils,
  Classes;

var
  // Accumulate/hide whitespace at EOL (at end of wrapped rows, actually)
  OldWhitespaceBehaviour: Boolean = False;

type
  TLineIndex = 0..MaxListSize;
  TRowIndex = 0..MaxListSize;
  TRowLength = byte;

  TRowIndexArray = array [TLineIndex] of TRowIndex;
  PRowIndexArray = ^TRowIndexArray;

  TRowLengthArray = array [TRowIndex] of TRowLength;
  PRowLengthArray = ^TRowLengthArray;

  {$IFNDEF SYN_COMPILER_4_UP}
  TSysCharSet = set of Char;
  {$ENDIF}

  // For clarity, I'll refer to buffer coordinates as 'Line' and
  // 'Char' and to display (wrapped) coordinates as 'Row' and 'Column'.

  // fLineOffsets[n] is the index of the first row of the [n+1]th line.
  // e.g. Starting row of first line (0) is 0. Starting row of second line (1)
  // is fLineOffsets[0]. Clear?

  TSynWordWrapPlugin = class(TInterfacedObject, ISynEditBufferPlugin)
  private
    fLineOffsets: PRowIndexArray;
    fRowLengths: PRowLengthArray;
    fLineCapacity: integer;
    fRowCapacity: integer;
    fLineCount: integer;
    //
    fEditor: TCustomSynEdit;
    fMinRowLength: TRowLength;
    fMaxRowLength: TRowLength;
    fBreakChars: TSysCharSet;
    procedure GrowLines(aMinSize: integer);
    procedure MoveLines(aStart: TLineIndex; aMoveBy: integer);
    procedure GrowRows(aMinSize: integer);
    procedure MoveRows(aStart: TRowIndex; aMoveBy: integer);
    procedure SetEmpty;
  protected
    procedure WrapLines;
    function ReWrapLine(aIndex: TLineIndex): integer;
    procedure TrimArrays;
    property LineOffsets: PRowIndexArray read fLineOffsets;
    property RowLengths: PRowLengthArray read fRowLengths;
    property Editor: TCustomSynEdit read fEditor;
  public
    constructor Create(aOwner: TCustomSynEdit);
    destructor Destroy; override;
    property LineCount: integer read fLineCount;
    { ISynEditBufferPlugin }
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: integer;
    function GetRowLength(aRow: integer): integer;
    function LinesInserted(aIndex: integer; aCount: integer): integer;
    function LinesDeleted(aIndex: integer; aCount: integer): integer;
    function LinesPutted(aIndex: integer; aCount: integer): integer;
    procedure Reset;
    procedure DisplayChanged; 
  end;

implementation

uses
{$IFDEF SYN_COMPILER_6_UP}
  RTLConsts,
{$ELSE}
  {$IFDEF SYN_CLX}
    QConsts;
  {$ELSE}
    Consts,
  {$ENDIF}
{$ENDIF}
{$IFNDEF SYN_COMPILER_4_UP}
  SynEditMiscProcs,
{$ENDIF}
  Math;

{ TSynWordWrapPlugin }

function TSynWordWrapPlugin.BufferToDisplayPos(
  const aPos: TBufferCoord): TDisplayCoord;
var
  vStartRow: integer; // first row of the line
  cRow: integer;
  vRowLen: integer;
begin
  Assert( aPos.Char > 0 );
  Assert( aPos.Line > 0 );
  if LineCount < aPos.Line then
  begin
    // beyond EOF
    Result.Column := aPos.Char;
    Result.Row := RowCount + ( aPos.Line - LineCount );
    Exit;
  end;
  if aPos.Line = 1 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[ aPos.Line -2 ];
  vRowLen := 0; 
  for cRow := vStartRow to fLineOffsets[ aPos.Line -1 ] -1 do
  begin
    Inc( vRowLen, fRowLengths[cRow] );
    if aPos.Char <= vRowLen then
    begin
      Result.Column := aPos.Char - vRowLen + fRowLengths[cRow];
      Result.Row := cRow +1;
      Exit;
    end;
  end;
  // beyond EOL
  Result.Column := aPos.Char - vRowLen + fRowLengths[ fLineOffsets[ aPos.Line -1 ] -1 ];
  Result.Row := fLineOffsets[ aPos.Line -1 ];
end;

constructor TSynWordWrapPlugin.Create(aOwner: TCustomSynEdit);
begin
  inherited Create; // just to work as reminder in case I revert it to a TComponent... 
  if aOwner = nil then
    raise Exception.Create( 'Owner of TSynWordWrapPlugin must be a TCustomSynEdit' );
  fEditor := aOwner;
  Reset;
end;

destructor TSynWordWrapPlugin.Destroy;
begin
  inherited;
  FreeMem( fLineOffsets );
  FreeMem( fRowLengths );
end;

procedure TSynWordWrapPlugin.DisplayChanged;
begin
  if Editor.CharsInWindow <> fMaxRowLength then
    Reset;
end;

function TSynWordWrapPlugin.DisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
var
  cLine: integer;
  cRow: integer;
begin
  Assert( aPos.Column > 0 );
  Assert( aPos.Row > 0 );
  if aPos.Row > RowCount then
  begin
    // beyond EOF
    Result.Char := aPos.Column;
    Result.Line := aPos.Row - RowCount + LineCount;
    Exit;
  end;
  //todo: use a binary search or something smarter
  for cLine := LineCount -2 downto 0 do
    if aPos.Row > fLineOffsets[cLine] then
    begin
      Result.Line := cLine +2;
      if aPos.Row = fLineOffsets[cLine + 1] then //last row of line
        Result.Char := Min(aPos.Column, fMaxRowLength + 1)
      else
        Result.Char := Min(aPos.Column, fRowLengths[aPos.Row - 1] + 1);
      for cRow := fLineOffsets[cLine] to aPos.Row -2 do
        Inc(Result.Char, fRowLengths[cRow]);
      Exit;
    end;
  // first line
  Result.Line := 1;
  if aPos.Row = fLineOffsets[0] then //last row of line
    Result.Char := Min(aPos.Column, fMaxRowLength + 1)
  else
    Result.Char := Min(aPos.Column, fRowLengths[aPos.Row - 1] + 1);
  for cRow := 0 to aPos.Row - 2 do
    Inc(Result.Char, fRowLengths[cRow]);
end;

function TSynWordWrapPlugin.GetRowLength(aRow: integer): integer;
// aRow is 1-based...
begin
  if (aRow <= 0) or (aRow > RowCount) then
    TList.Error( SListIndexError, aRow );
  Result := fRowLengths[ aRow -1 ];
end;

procedure TSynWordWrapPlugin.GrowLines(aMinSize: integer);
const
  vStepSize = 256;
begin
  Assert( aMinSize > 0 );
  if aMinSize > fLineCapacity then
  begin
    aMinSize := aMinSize + vStepSize - (aMinSize mod vStepSize);
    ReallocMem( fLineOffsets, aMinSize * SizeOf(TRowIndex) );
    fLineCapacity := aMinSize;
  end;
end;

procedure TSynWordWrapPlugin.GrowRows(aMinSize: integer);
const
  vStepSize = 512;
begin
  Assert( aMinSize > 0 );
  if aMinSize > fRowCapacity then
  begin
    aMinSize := aMinSize + vStepSize - (aMinSize mod vStepSize);
    ReallocMem( fRowLengths, aMinSize * SizeOf(TRowLength) );
    fRowCapacity := aMinSize;
  end;
end;

function TSynWordWrapPlugin.LinesDeleted(aIndex: integer; aCount: integer): integer;
var
  vStartRow: integer;
  vEndRow: integer;
  cLine: integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert( aIndex >= 0 );
  Assert( aCount >= 1 );
  Assert( aIndex + aCount <= LineCount );
  //
  if aIndex = 0 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[ aIndex -1 ];
  vEndRow := fLineOffsets[ aIndex + aCount -1 ];
  Result := vEndRow - vStartRow;
  // resize fRowLengths
  if vEndRow < RowCount then
    MoveRows( vEndRow, - Result );
  // resize fLineOffsets
  MoveLines( aIndex + aCount, - aCount );
  Dec( fLineCount, aCount );
  // update offsets
  for cLine := aIndex to LineCount -1 do
    Dec( fLineOffsets[cLine], Result );
end;

function TSynWordWrapPlugin.LinesInserted(aIndex: integer; aCount: integer): integer;
var
  vPrevOffset: TRowIndex;
  cLine: integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert( aIndex >= 0 );
  Assert( aCount >= 1 );
  Assert( aIndex <= LineCount );
  // resize fLineOffsets
  GrowLines( LineCount + aCount );
  if aIndex < LineCount then // no need for MoveLines if inserting at LineCount (TSynEditStringList.Add)
  begin
    Inc( fLineCount, aCount ); // fLineCount must be updated before calling MoveLines()
    MoveLines( aIndex, aCount );
  end
  else
    Inc( fLineCount, aCount ); 
  // set offset to same as previous line (i.e. the line has 0 rows)
  if aIndex = 0 then
    vPrevOffset := 0
  else
    vPrevOffset := fLineOffsets[ aIndex -1 ];
  for cLine := aIndex to aIndex + aCount -1 do
    fLineOffsets[cLine] := vPrevOffset;
  // Rewrap.
  Result := 0;
  for cLine := aIndex to aIndex + aCount -1 do
    Inc( Result, ReWrapLine(cLine) );
end;

function TSynWordWrapPlugin.LinesPutted(aIndex: integer; aCount: integer): integer;
var
  cLine: integer;
begin
  if fMaxRowLength = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Assert( aIndex >= 0 );
  Assert( aCount >= 1 );
  Assert( aIndex + aCount <= LineCount );
  // Rewrap.
  Result := 0;
  for cLine := aIndex to aIndex + aCount -1 do
    Inc( Result, ReWrapLine(cLine) );
end;

procedure TSynWordWrapPlugin.MoveLines(aStart: TLineIndex; aMoveBy: integer);
var
  vMoveCount: integer;
begin
  Assert( aMoveBy <> 0 );
  Assert( aStart + aMoveBy >= 0 );
  Assert( aStart + aMoveBy < LineCount );
  vMoveCount := LineCount - aStart;
  if aMoveBy > 0 then
    Dec( vMoveCount, aMoveBy );
  Move( fLineOffsets[aStart], fLineOffsets[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowIndex) );
end;

procedure TSynWordWrapPlugin.MoveRows(aStart: TRowIndex; aMoveBy: integer);
var
  vMoveCount: integer;
begin
  Assert( aMoveBy <> 0 );
  Assert( aStart + aMoveBy >= 0 );
  Assert( aStart + aMoveBy < RowCount );
  vMoveCount := RowCount - aStart;
  if aMoveBy > 0 then
    Dec( vMoveCount, aMoveBy );
  Move( fRowLengths[aStart], fRowLengths[aStart + aMoveBy],
    vMoveCount * SizeOf(TRowLength) );
end;

procedure TSynWordWrapPlugin.Reset;
begin
  Assert( Editor.CharsInWindow >= 0 );

  fMaxRowLength := Editor.CharsInWindow;
  fMinRowLength := Editor.CharsInWindow - (Editor.CharsInWindow div 3);
  fBreakChars :=  [#0..#255] - Editor.IdentChars;

  if fMinRowLength <= 0 then
    fMinRowLength := 1;

  WrapLines;
end;

function TSynWordWrapPlugin.ReWrapLine(aIndex: TLineIndex): integer;
// Returns RowCount delta (how many wrapped lines were added or removed by this change).
var
  vMaxNewRows: Cardinal;
  vLine: string;
  vLineRowCount: Integer; //numbers of rows parsed in this line
  vTempRowLengths: PRowLengthArray;
  vRowBegin: PChar;
  vLineEnd: PChar;
  vRowEnd: PChar;
  vRunner: PChar;
  vRowMinEnd: PChar;
  //
  vStartRow: Integer; // first row of the line
  vOldNextRow: Integer; // first row of the next line, before the change
  cLine: Integer;
begin
  // ****** First parse the new string using an auxiliar array *****
  vLine := TSynEditStringList(Editor.Lines).ExpandedStrings[aIndex];
  // Pre-allocate a buffer for rowlengths
  vMaxNewRows := ((Length(vLine) - 1) div fMinRowLength) + 1;
  vTempRowLengths := AllocMem(vMaxNewRows);
  try
    vLineRowCount := 0;
    vRowBegin := PChar(vLine);
    vRowEnd := vRowBegin + fMaxRowLength;
    vLineEnd := vRowBegin + Length(vLine);
    while vRowEnd < vLineEnd do
    begin
      //
      if OldWhitespaceBehaviour and (vRowEnd^ in [#32, #9]) then
      begin
        repeat
          Inc(vRowEnd);
        until not(vRowEnd^ in [#32, #9]);
      end
      else begin
        vRowMinEnd := vRowBegin + fMinRowLength;
        vRunner := vRowEnd;
        while vRunner > vRowMinEnd do
        begin
          if vRunner^ in fBreakChars then
          begin
            vRowEnd := vRunner;
            break;
          end;
          Dec(vRunner);
        end;
      end;
      // Check TRowLength overflow
      if OldWhitespaceBehaviour and (vRowEnd - vRowBegin > High(TRowLength)) then
      begin
        vRowEnd := vRowBegin + High(TRowLength);
        vRowMinEnd := vRowEnd - (High(TRowLength) mod Editor.TabWidth);
        while (vRowEnd^ = #9) and (vRowEnd > vRowMinEnd) do
          Dec(vRowEnd);
      end;
{$IFDEF SYN_MBCSSUPPORT}
      if StrByteType(vRowBegin, vRowEnd - vRowBegin) = mbTrailByte then
        Dec(vRowEnd);
{$ENDIF}
      // Finally store the rowlength
      vTempRowLengths[vLineRowCount] := vRowEnd - vRowBegin;
      //
      Inc(vLineRowCount);
      vRowBegin := vRowEnd;
      Inc(vRowEnd, fMaxRowLength);
    end; //endwhile vRowEnd < vLineEnd
    if (vLineEnd > vRowBegin) or (Length(vLine) = 0) then
    begin
      vTempRowLengths[vLineRowCount] := vLineEnd - vRowBegin;
      Inc(vLineRowCount);
    end;

    // ****** Then updates the main arrays ******
    if aIndex = 0 then
      vStartRow := 0
    else
      vStartRow := fLineOffsets[aIndex - 1];
    vOldNextRow := fLineOffsets[aIndex];
    Result := vLineRowCount - (vOldNextRow - vStartRow);
    if Result <> 0 then
    begin
      // MoveRows depends on RowCount, so we need some special processing...
      if Result > 0 then
      begin
        // ...if growing, update offsets (and thus RowCount) before rowlengths
        GrowRows(RowCount + Result);
        for cLine := aIndex to LineCount - 1 do
          Inc(fLineOffsets[cLine], Result);
        if vOldNextRow < RowCount - Result then
          MoveRows(vOldNextRow, Result);
      end
      else begin
        // ...if shrinking, update offsets after rowlengths
        if vOldNextRow < RowCount then
          MoveRows(vOldNextRow, Result);
        for cLine := aIndex to LineCount - 1 do
          Inc(fLineOffsets[cLine], Result);
      end;
    end;
    Move(vTempRowLengths[0], fRowLengths[vStartRow], vLineRowCount * SizeOf(TRowLength));
  finally
    FreeMem(vTempRowLengths);
  end;
end;

procedure TSynWordWrapPlugin.WrapLines;
var
  cRow: Integer;
  cLine: Integer;
  vLine: string;
  vMaxNewRows: Integer;
  vRowBegin: PChar;
  vLineEnd: PChar;
  vRowEnd: PChar;
  vRunner: PChar;
  vRowMinEnd: PChar;
begin
  if (Editor.Lines.Count = 0) or (fMaxRowLength <= 0) then
  begin
    SetEmpty;
    Exit;
  end;

  GrowLines(Editor.Lines.Count);
  GrowRows(Editor.Lines.Count);
  //
  cRow := 0;
  for cLine := 0 to Editor.Lines.Count -1 do
  begin
    vLine := TSynEditStringList(Editor.Lines).ExpandedStrings[cLine];
    //
    vMaxNewRows := ((Length(vLine) - 1) div fMinRowLength) + 1;
    GrowRows(cRow + vMaxNewRows);
    //
    vRowBegin := PChar(vLine);
    vRowEnd := vRowBegin + fMaxRowLength;
    vLineEnd := vRowBegin + Length(vLine);
    while vRowEnd < vLineEnd do
    begin
      //
      if OldWhitespaceBehaviour and (vRowEnd^ in [#32, #9]) then
      begin
        repeat
          Inc(vRowEnd);
        until not(vRowEnd^ in [#32, #9]);
      end
      else begin
        vRowMinEnd := vRowBegin + fMinRowLength;
        vRunner := vRowEnd;
        while vRunner > vRowMinEnd do
        begin
          if vRunner^ in fBreakChars then
          begin
            vRowEnd := vRunner;
            break;
          end;
          Dec(vRunner);
        end;
      end;
      //
      if OldWhitespaceBehaviour and (vRowEnd - vRowBegin > High(TRowLength)) then
      begin
        vRowEnd := vRowBegin + High(TRowLength);
        vRowMinEnd := vRowEnd - (High(TRowLength) mod Editor.TabWidth);
        while (vRowEnd^ = #9) and (vRowEnd > vRowMinEnd) do
          Dec(vRowEnd);
      end;
{$IFDEF SYN_MBCSSUPPORT}
      if StrByteType(vRowBegin, vRowEnd - vRowBegin) = mbTrailByte then
        Dec(vRowEnd);
{$ENDIF}
      fRowLengths[cRow] := vRowEnd - vRowBegin;
      //
      Inc(cRow);
      vRowBegin := vRowEnd;
      Inc(vRowEnd, fMaxRowLength);
    end;
    if (vLineEnd > vRowBegin) or (Length(vLine) = 0) then
    begin
      fRowLengths[cRow] := vLineEnd - vRowBegin;
      Inc(cRow);
    end;
    fLineOffsets[cLine] := cRow;
  end;
  fLineCount := Editor.Lines.Count;
end;

function TSynWordWrapPlugin.RowCount: integer;
begin
  if LineCount > 0 then
    Result := fLineOffsets[ LineCount -1 ]
  else
    Result := 0;
end;

procedure TSynWordWrapPlugin.SetEmpty;
begin
  fLineCount := 0;
  // free unsused memory
  TrimArrays;
end;

procedure TSynWordWrapPlugin.TrimArrays;
begin
  ReallocMem( fLineOffsets, LineCount * SizeOf(TRowIndex) );
  fLineCapacity := LineCount;
  ReallocMem( fRowLengths, RowCount * SizeOf(TRowLength) );
  fRowCapacity := RowCount;
end;

end.
