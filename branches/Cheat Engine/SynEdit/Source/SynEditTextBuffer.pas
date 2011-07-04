{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTextBuffer.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
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

$Id: SynEditTextBuffer.pas,v 1.70 2007/03/14 03:06:26 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
//todo: Avoid calculating expanded string unncessarily (just calculate expandedLength instead).

{$IFNDEF QSYNEDITTEXTBUFFER}
unit SynEditTextBuffer;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
  QSynEditTypes,
  QSynEditMiscProcs,
{$ELSE}
  Windows,
  SynEditTypes,
  SynEditMiscProcs,
{$ENDIF}
  SysUtils,
  Classes;

type
  TSynEditRange = pointer;

  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TSynEditStringFlags = set of TSynEditStringFlag;

  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = record
    fString: string;
    fObject: TObject;
    fRange: TSynEditRange;
    fExpandedLength: integer;
    fFlags: TSynEditStringFlags;
  end;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array[0..MaxSynEditStrings - 1] of TSynEditStringRec;

  TStringListChangeEvent = procedure(Sender: TObject; Index: Integer;
    Count: integer) of object;

  TSynEditFileFormat = (sffDos, sffUnix, sffMac); // DOS: CRLF, UNIX: LF, Mac: CR

  TSynEditStringList = class(TStrings)
  private
    fList: PSynEditStringRecList;
    fCount: integer;
    fCapacity: integer;
    fFileFormat: TSynEditFileFormat;
    fAppendNewLineAtEOF: Boolean;
    fConvertTabsProc: TConvertTabsProcEx;
    fIndexOfLongestLine: integer;
    fTabWidth: integer;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TStringListChangeEvent;
    fOnInserted: TStringListChangeEvent;
    fOnPutted: TStringListChangeEvent;
    function ExpandString(Index: integer): string;
    function GetExpandedString(Index: integer): string;
    function GetExpandedStringLength(Index: integer): integer;
    function GetLengthOfLongestLine: integer;
    function GetRange(Index: integer): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    procedure PutRange(Index: integer; ARange: TSynEditRange);
  protected
    function Get(Index: integer): string; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    function GetTextStr: string; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
    procedure SetTabWidth(Value: integer);
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);
    procedure Exchange(Index1, Index2: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer);
    procedure InsertStrings(Index: integer; NewStrings: TStrings);
    procedure InsertText(Index: integer; NewText: String);
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property AppendNewLineAtEOF: Boolean read fAppendNewLineAtEOF write fAppendNewLineAtEOF;
    property FileFormat: TSynEditFileFormat read fFileFormat write fFileFormat;
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property ExpandedStringLengths[Index: integer]: integer read GetExpandedStringLength;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: integer read fTabWidth write SetTabWidth;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TStringListChangeEvent read fOnDeleted write fOnDeleted;
    property OnInserted: TStringListChangeEvent read fOnInserted
      write fOnInserted;
    property OnPutted: TStringListChangeEvent read fOnPutted write fOnPutted;
  end;

  ESynEditStringList = class(Exception);

  TSynChangeReason = (crInsert, crPaste, crDragDropInsert,
    //several undo entries can be chained together via the ChangeNumber
    //see also TCustomSynEdit.[Begin|End]UndoBlock methods
    crDeleteAfterCursor, crDelete,
    crLineBreak, crIndent, crUnindent,
    crSilentDelete, crSilentDeleteAfterCursor,
    crAutoCompleteBegin, crAutoCompleteEnd,
    crPasteBegin, crPasteEnd, //for pasting, since it might do a lot of operations
    crSpecial1Begin, crSpecial1End,
    crSpecial2Begin, crSpecial2End,
    crCaret,      //just restore the Caret, allowing better Undo behavior
    crSelection,  //restore Selection
    crNothing,
    crGroupBreak,
    crDeleteAll
    );

  TSynEditUndoItem = class(TPersistent)
  protected
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TBufferCoord;
    fChangeEndPos: TBufferCoord;
    fChangeStr: string;
    fChangeNumber: integer;
  public
    procedure Assign(Source: TPersistent); override;
    property ChangeReason: TSynChangeReason read fChangeReason;
    property ChangeSelMode: TSynSelectionMode read fChangeSelMode;
    property ChangeStartPos: TBufferCoord read fChangeStartPos;
    property ChangeEndPos: TBufferCoord read fChangeEndPos;
    property ChangeStr: string read fChangeStr;
    property ChangeNumber: integer read fChangeNumber;
  end;

  TSynEditUndoList = class(TPersistent)
  protected
    fBlockChangeNumber: integer;
    fBlockCount: integer;
    fFullUndoImposible: boolean;
    fItems: TList;
    fLockCount: integer;
    fMaxUndoActions: integer;
    fNextChangeNumber: integer;
    fInitialChangeNumber: integer;
    fInsideRedo: boolean;
    fOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetItemCount: integer;
    procedure SetMaxUndoActions(Value: integer);
    procedure SetInitialState(const Value: boolean);
    function GetInitialState: boolean;
    function GetItems(Index: Integer): TSynEditUndoItem;
    procedure SetItems(Index: Integer; const Value: TSynEditUndoItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AReason: TSynChangeReason; const AStart, AEnd: TBufferCoord;
      const ChangeText: string; SelMode: TSynSelectionMode);
    procedure BeginBlock;
    procedure Clear;
    procedure EndBlock;
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
    function LastChangeReason: TSynChangeReason;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddGroupBreak;
    procedure DeleteItem(AIndex: Integer);
    property BlockChangeNumber: integer read fBlockChangeNumber
      write fBlockChangeNumber;
    property CanUndo: boolean read GetCanUndo;
    property FullUndoImpossible: boolean read fFullUndoImposible;
    property InitialState: boolean read GetInitialState write SetInitialState;
    property Items[Index: Integer]: TSynEditUndoItem read GetItems write SetItems;
    property ItemCount: integer read GetItemCount;
    property BlockCount: integer read fBlockCount;
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property InsideRedo: boolean read fInsideRedo write fInsideRedo;
    property OnAddedUndo: TNotifyEvent read fOnAddedUndo write fOnAddedUndo;
  end;

implementation

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SListIndexOutOfBounds = 'Invalid stringlist index %d';
  SInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

{ TSynEditFiler }

type
  TSynEditFiler = class(TObject)
  protected
    fBuffer: PChar;
    fBufPtr: Cardinal;
    fBufSize: Cardinal;
    fFileFormat: TSynEditFileFormat;
    fFiler: TFileStream;
    procedure Flush; virtual;
    procedure SetBufferSize(NewSize: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property FileFormat: TSynEditFileFormat read fFileFormat write fFileFormat;
  end;

constructor TSynEditFiler.Create;
const
  kByte = 1024;
begin
  inherited Create;
  fFileFormat := sffUnix;
  SetBufferSize(16 * kByte);
  fBuffer[0] := #0;
end;

destructor TSynEditFiler.Destroy;
begin
  Flush;
  fFiler.Free;
  SetBufferSize(0);
  inherited Destroy;
end;

procedure TSynEditFiler.Flush;
begin
end;

procedure TSynEditFiler.SetBufferSize(NewSize: Cardinal);
begin
  if NewSize <> fBufSize then begin
    ReallocMem(fBuffer, NewSize);
    fBufSize := NewSize;
  end;
end;

{ TSynEditFileReader }

(*
type
  TSynEditFileReader = class(TSynEditFiler)
  protected
    fFilePos: Cardinal;
    fFileSize: Cardinal;
    procedure FillBuffer;
  public
    constructor Create(const FileName: string);
    function EOF: boolean;
    function ReadLine: string;
  end;

constructor TSynEditFileReader.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  fFileSize := fFiler.Size;
  fFiler.Seek(0, soFromBeginning);
end;

function TSynEditFileReader.EOF: boolean;
begin
  Result := (fBuffer[fBufPtr] = #0) and (fFilePos >= fFileSize);
end;

procedure TSynEditFileReader.FillBuffer;
var
  Count: Cardinal;
begin
  if fBufPtr >= fBufSize - 1 then
    fBufPtr := 0;
  Count := fFileSize - fFilePos;
  if Count >= fBufSize - fBufPtr then
    Count := fBufSize - fBufPtr - 1;
  fFiler.ReadBuffer(fBuffer[fBufPtr], Count);
  fBuffer[fBufPtr + Count] := #0;
  fFilePos := fFilePos + Count;
  fBufPtr := 0;
end;

function TSynEditFileReader.ReadLine: string;
var
  E, P, S: PChar;
begin
  repeat
    S := PChar(@fBuffer[fBufPtr]);
    if S[0] = #0 then begin
      FillBuffer;
      S := PChar(@fBuffer[0]);
    end;
    E := PChar(@fBuffer[fBufSize]);
    P := S;
    while P + 2 < E do begin
      case P[0] of
        #10, #13:
          begin
            SetString(Result, S, P - S);
            if P[0] = #13 then
            begin
              if P[1] = #10 then
              begin
                fFileFormat := sffDos;
                Inc(P);
              end
              else
                fFileFOrmat := sffMac;
            end;
            Inc(P);
            fBufPtr := P - fBuffer;
            exit;
          end;
        #0:
          if fFilePos >= fFileSize then begin
            fBufPtr := P - fBuffer;
            SetString(Result, S, P - S);
            exit;
          end;
      end;
      Inc(P);
    end;
    // put the partial string to the start of the buffer, and refill the buffer
    Inc(P);
    if S > fBuffer then
      StrLCopy(fBuffer, S, P - S);
    fBufPtr := P - S;
    fBuffer[fBufPtr] := #0;
    // if line is longer than half the buffer then grow it first
    if 2 * Cardinal(P - S) > fBufSize then
      SetBufferSize(fBufSize + fBufSize);
  until FALSE;
end;
*)

{ TSynEditFileWriter }

type
  TSynEditFileWriter = class(TSynEditFiler)
  protected
    procedure Flush; override;
  public
    constructor Create(const FileName: string);
    procedure WriteLine(const S: string; const FileFormat: TSynEditFileFormat);
    procedure Write(const S: String);
  end;

constructor TSynEditFileWriter.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStream.Create(FileName, fmCreate);
  fFiler.Seek(0, soFromBeginning);
end;

procedure TSynEditFileWriter.Flush;
begin
  if fBufPtr > 0 then begin
    fFiler.WriteBuffer(fBuffer[0], fBufPtr);
    fBufPtr := 0;
  end;
end;

procedure TSynEditFileWriter.Write(const S: String);
var L: Cardinal;
begin
  L := Length(S);
  repeat
    if fBufPtr + L <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      exit;
    end;
    Flush;
    if L > fBufSize then
      SetBufferSize(L);
  until False;
end;

procedure TSynEditFileWriter.WriteLine(const S: string;
  const FileFormat: TSynEditFileFormat);
var
  L, NL: Cardinal;
begin
  L := Length(S);
  NL := 1 + Ord(fFileFormat = sffDos);
  repeat
    if fBufPtr + L + NL <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      if (fFileFormat <> sffUnix) then
      begin
        fBuffer[fBufPtr] := #13; // CR
        Inc(fBufPtr);
      end;
      if (fFileFormat <> sffMac) then
      begin
        fBuffer[fBufPtr] := #10; // LF
        Inc(fBufPtr);
      end;
      Exit;
    end;
    Flush;
    if L + NL > fBufSize then
      SetBufferSize(L + NL);
  until False;
end;

{ TSynEditStringList }

procedure ListIndexOutOfBounds(Index: integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create;
begin
  inherited Create;
  fFileFormat := sffDos;
  fIndexOfLongestLine := -1;
  TabWidth := 8;
end;

destructor TSynEditStringList.Destroy;
begin
  fOnChange := nil;
  fOnChanging := nil;
  inherited Destroy;
  if fCount <> 0 then
    Finalize(fList^[0], fCount);
  fCount := 0;
  SetCapacity(0);
end;

function TSynEditStringList.Add(const S: string): integer;
begin
  BeginUpdate;
  Result := fCount;
  InsertItem(Result, S);
  if Assigned(OnInserted) then
    OnInserted( Self, Result, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(Strings: TStrings);
var
  i, FirstAdded: integer;
begin
  if Strings.Count > 0 then begin
    fIndexOfLongestLine := -1;
    BeginUpdate;
    try
      i := fCount + Strings.Count;
      if i > fCapacity then
        SetCapacity((i + 15) and (not 15));
      FirstAdded := fCount;
      for i := 0 to Strings.Count - 1 do begin
        with fList^[fCount] do begin
          Pointer(fString) := nil;
          fString := Strings[i];
          fObject := Strings.Objects[i];
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
        Inc(fCount);
      end;
      if Assigned(OnInserted) then
        OnInserted( Self, FirstAdded, Strings.Count );
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.Clear;
begin
  if fCount <> 0 then begin
    BeginUpdate;
    Finalize(fList^[0], fCount);
    fCount := 0;
    SetCapacity(0);
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  Finalize(fList^[Index]);
  Dec(fCount);
  if Index < fCount then begin
    System.Move(fList^[Index + 1], fList^[Index],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;
  if Assigned(fOnDeleted) then
    fOnDeleted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: integer;
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index > fCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := fCount - (Index + NumLines - 1);
    if LinesAfter < 0 then
      NumLines := fCount - Index - 1;
    Finalize(fList^[Index], NumLines);

    if LinesAfter > 0 then begin
      BeginUpdate;
      try
        System.Move(fList^[Index + NumLines], fList^[Index],
          LinesAfter * SynEditStringRecSize);
      finally
        EndUpdate;
      end;
    end;
    Dec(fCount, NumLines);
    if Assigned(fOnDeleted) then
      fOnDeleted( Self, Index, NumLines );
  end;
end;

procedure TSynEditStringList.Exchange(Index1, Index2: integer);
var
  Temp: TSynEditStringRec;
begin
  if (Index1 < 0) or (Index1 >= fCount) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= fCount) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  Temp := fList^[Index1];
  fList^[Index1] := fList^[Index2];
  fList^[Index2] := Temp;
  if fIndexOfLongestLine = Index1 then
    fIndexOfLongestLine := Index2
  else if fIndexOfLongestLine = Index2 then
    fIndexOfLongestLine := Index1;
  EndUpdate;
end;

function TSynEditStringList.ExpandString(Index: integer): string;
var
  HasTabs: boolean;
begin
  with fList^[Index] do
    if fString = '' then begin
      Result := '';
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Include(fFlags, sfHasNoTabs);
      fExpandedLength := 0;
    end else begin
      Result := fConvertTabsProc(fString, fTabWidth, HasTabs);
      fExpandedLength := Length(Result);
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      if HasTabs then
        Include(fFlags, sfHasTabs)
      else
        Include(fFlags, sfHasNoTabs);
    end;
end;

function TSynEditStringList.Get(Index: integer): string;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fString
  else
    Result := '';
end;

function TSynEditStringList.GetCapacity: integer;
begin
  Result := fCapacity;
end;

function TSynEditStringList.GetCount: integer;
begin
  Result := fCount;
end;

function TSynEditStringList.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < fCount) then begin
    if sfHasNoTabs in fList^[Index].fFlags then
      Result := fList^[Index].fString
    else
      Result := ExpandString(Index);
  end else
    Result := '';
end;

function TSynEditStringList.GetExpandedStringLength(Index: integer): integer;
begin
  if (Index >= 0) and (Index < fCount) then
  begin
    if sfExpandedLengthUnknown in fList^[Index].fFlags then
      Result := Length( ExpandedStrings[index] )
    else
      Result := fList^[Index].fExpandedLength;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetLengthOfLongestLine: integer;
var
  i, MaxLen: integer;
  PRec: PSynEditStringRec;
begin
  if fIndexOfLongestLine < 0 then begin
    MaxLen := 0;
    if fCount > 0 then begin
      PRec := @fList^[0];
      for i := 0 to fCount - 1 do begin
        if sfExpandedLengthUnknown in PRec^.fFlags then
          ExpandString(i);
        if PRec^.fExpandedLength > MaxLen then begin
          MaxLen := PRec^.fExpandedLength;
          fIndexOfLongestLine := i;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < fCount) then
    Result := fList^[fIndexOfLongestLine].fExpandedLength
  else
    Result := 0;
end;

function TSynEditStringList.GetObject(Index: integer): TObject;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fRange
  else
    Result := nil;
end;

function TSynEditStringList.GetTextStr: string;
begin
  Result := inherited GetTextStr;
  System.Delete(Result, Length(Result) - Length(SLineBreak) + 1, MaxInt);
end;

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if fCapacity > 64 then
    Delta := fCapacity //fCapacity div 4
  else
    Delta := 16;
  SetCapacity(fCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: integer; const S: string);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(fOnInserted) then
    fOnInserted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: integer; const S: string);
begin
  BeginUpdate;
  if fCount = fCapacity then
    Grow;
  if Index < fCount then begin
    System.Move(fList^[Index], fList^[Index + 1],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;
  with fList^[Index] do begin
    Pointer(fString) := nil;
    fString := S;
    fObject := nil;
    fRange := NullRange;
    fExpandedLength := -1;
    fFlags := [sfExpandedLengthUnknown];
  end;
  Inc(fCount);
  EndUpdate;
end;

procedure TSynEditStringList.InsertLines(Index, NumLines: integer);
var
	c_Line: Integer;
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then begin
    BeginUpdate;
    try
      SetCapacity(fCount + NumLines);
      if Index < fCount then begin
        System.Move(fList^[Index], fList^[Index + NumLines],
          (fCount - Index) * SynEditStringRecSize);
      end;
      for c_Line := Index to Index + NumLines -1 do
			  with fList^[c_Line] do 
				begin
			    Pointer(fString) := nil;
			    fObject := nil;
			    fRange := NullRange;
			    fExpandedLength := -1;
			    fFlags := [sfExpandedLengthUnknown];
			  end;
      Inc(fCount, NumLines);
      if Assigned(OnInserted) then
        OnInserted( Self, Index, NumLines );
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: integer;
  NewStrings: TStrings);
var
  i, Cnt: integer;
begin
  Cnt := NewStrings.Count;
  if Cnt = 0 then exit;

  BeginUpdate;
  try
    InsertLines(Index, Cnt);
    for i := 0 to Cnt - 1 do
      Strings[Index + i] := NewStrings[i];
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.InsertText(Index: integer;
  NewText: String);
var
  TmpStringList: TStringList;
begin
  if NewText = '' then exit;

  TmpStringList := TStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(Index, TmpStringList);
  finally
    TmpStringList.Free;
  end;
end;

procedure TSynEditStringList.LoadFromFile(const FileName: string);
var
//  Reader: TSynEditFileReader;
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;

(*//Old Code, for reference
  Reader := TSynEditFileReader.Create(FileName);
  try
    BeginUpdate;
    try
      Clear;
      while not Reader.EOF do
        Add(Reader.ReadLine);
      fFileFormat := Reader.FileFormat;
    finally
      EndUpdate;
    end;
  finally
    Reader.Free;
  end;
*)
end;

procedure TSynEditStringList.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S, S1: string;
  P, Start: PChar;
  fCR, fLF: Boolean;
  iPos: Integer;
begin
  fCR := False;
  fLF := False;
  try
    BeginUpdate;
    Size := Stream.Size;
    Stream.Position := 0;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);

    Clear;
    P := Pointer(S);
    if P <> nil then
    begin
      iPos := 0;
      while (iPos < Size) do // (P^ <> #0) do
      begin
        Start := P;
        while not (P^ in [#10, #13]) and (iPos < Size) do
        begin
          Inc(P);
          Inc(iPos);
        end;
        SetString(S1, Start, P - Start);
        Add(S1);
        if (P^ = #13) then
        begin
          fCR := True;
          Inc(P);
          Inc(iPos);
        end;
        if (P^ = #10) then
        begin
          fLF := True;
          Inc(P);
          Inc(iPos);
        end;
      end;
      { keep the old format of the file }
      if (not AppendNewLineAtEOF) and (S[Size] in [#10,#13]) then
        Add('');
    end;
  finally
    EndUpdate;
  end;
  if fCR and not fLF then
    fFileFormat := sffMac
  else if fLF and not fCR then
    fFileFormat := sffUnix
  else
    fFileFormat := sffDos;
end;

procedure TSynEditStringList.SaveToStream(Stream: TStream);
var
  S, S1: string;
  I, L, Size: Integer;
  P: PChar;
  LineEndLength: Integer;
begin
  Size := 0;
  if FileFormat in [sffMac, sffUnix] then
    LineEndLength := 1
  else
    LineEndLength := 2;
  for I := 0 to Count - 1 do Inc(Size, Length(Strings[I]) + LineEndLength);
  if not AppendNewLineAtEOF then
    Dec( Size, LineEndLength );
  SetString(S, nil, Size);
  P := Pointer(S);
  for I := 0 to Count - 1 do begin
    S1 := Strings[I];
    L := Length(S1);
    if L <> 0 then
    begin
      System.Move(Pointer(S1)^, P^, L);
      Inc(P, L);
    end;
    //Do not add new line to last line
    if (I < Count-1) or (AppendNewLineAtEOF) then begin
      if FileFormat = sffMac then begin
        P^ := #13;
        Inc(P);
      end else
      if FileFormat = sffUnix then begin
        P^ := #10;
        Inc(P);
      end else begin
        P^ := #13;
        Inc(P);
        P^ := #10;
        Inc(P);
      end;
    end;
  end;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TSynEditStringList.Put(Index: integer; const S: string);
begin
  if (Index = 0) and (fCount = 0) or (fCount = Index) then
    Add(S)
  else begin
    if (Index < 0) or (Index >= fCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    fIndexOfLongestLine := -1;
    with fList^[Index] do begin
      Include(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      fString := S;
    end;
    if Assigned(fOnPutted) then
      fOnPutted( Self, Index, 1 );
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: integer; ARange: TSynEditRange);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fRange := ARange;
  EndUpdate;
end;

procedure TSynEditStringList.SaveToFile(const FileName: string);
var
  Writer: TSynEditFileWriter;
  i: integer;
  s: string;
begin
  Writer := TSynEditFileWriter.Create(FileName);
  try
    Writer.FileFormat := fFileFormat;
    i := 0;
    while i < fCount do begin
      s := Get(i);
      Inc(i);
      if (i<fCount) or (AppendNewLineAtEOF) then
        Writer.WriteLine(s, fFileFormat)
      else
        Writer.Write(s);
    end;
  finally
    Writer.Free;
  end;
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  if NewCapacity < Count then
    EListError.Create( SInvalidCapacity );
  ReallocMem(fList, NewCapacity * SynEditStringRecSize);
  fCapacity := NewCapacity;
end;

procedure TSynEditStringList.SetTabWidth(Value: integer);
var
  i: integer;
begin
  if Value <> fTabWidth then begin
    fTabWidth := Value;
    fConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
    fIndexOfLongestLine := -1;
    for i := 0 to fCount - 1 do
      with fList^[i] do begin
        fExpandedLength := -1;
        Exclude(fFlags, sfHasNoTabs);
        Include(fFlags, sfExpandedLengthUnknown);
      end;
  end;
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then begin
    if Assigned(fOnChanging) then
      fOnChanging(Self);
  end else begin
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

{ TSynEditUndoItem }

procedure TSynEditUndoItem.Assign(Source: TPersistent);
begin
  if (Source is TSynEditUndoItem) then
  begin
    fChangeReason:=TSynEditUndoItem(Source).fChangeReason;
    fChangeSelMode:=TSynEditUndoItem(Source).fChangeSelMode;
    fChangeStartPos:=TSynEditUndoItem(Source).fChangeStartPos;
    fChangeEndPos:=TSynEditUndoItem(Source).fChangeEndPos;
    fChangeStr:=TSynEditUndoItem(Source).fChangeStr;
    fChangeNumber:=TSynEditUndoItem(Source).fChangeNumber;
  end
  else
    inherited Assign(Source);
end;

{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
  fNextChangeNumber := 1;
  fInsideRedo := False;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TSynEditUndoList.Assign(Source: TPersistent);
var
  i: Integer;
  UndoItem: TSynEditUndoItem;
begin
  if (Source is TSynEditUndoList) then
  begin
    Clear;
    for i:=0 to TSynEditUndoList(Source).fItems.Count-1 do
    begin
      UndoItem:=TSynEditUndoItem.Create;
      UndoItem.Assign(TSynEditUndoList(Source).fItems[i]);
      fItems.Add(UndoItem);
    end;
    fBlockChangeNumber:=TSynEditUndoList(Source).fBlockChangeNumber;
    fBlockCount:=TSynEditUndoList(Source).fBlockCount;
    fFullUndoImposible:=TSynEditUndoList(Source).fFullUndoImposible;
    fLockCount:=TSynEditUndoList(Source).fLockCount;
    fMaxUndoActions:=TSynEditUndoList(Source).fMaxUndoActions;
    fNextChangeNumber:=TSynEditUndoList(Source).fNextChangeNumber;
    fInsideRedo:=TSynEditUndoList(Source).fInsideRedo;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; const AStart,
  AEnd: TBufferCoord; const ChangeText: string; SelMode: TSynSelectionMode);
var
  NewItem: TSynEditUndoItem;
begin
  if fLockCount = 0 then begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        fChangeReason := AReason;
        fChangeSelMode := SelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := ChangeText;
        if fBlockChangeNumber <> 0 then
          fChangeNumber := fBlockChangeNumber
        else begin
          fChangeNumber := fNextChangeNumber;
          if fBlockCount = 0 then begin
            Inc(fNextChangeNumber);
            if fNextChangeNumber = 0 then
              Inc(fNextChangeNumber);
          end;
        end;
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.BeginBlock;
begin
  Inc(fBlockCount);
  fBlockChangeNumber := fNextChangeNumber;
end;

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[i]).Free;
  fItems.Clear;
  fFullUndoImposible := False;
end;

procedure TSynEditUndoList.EndBlock;
var
  iBlockID: integer;
begin
  if fBlockCount > 0 then begin
    Dec(fBlockCount);
    if fBlockCount = 0 then begin
      iBlockID := fBlockChangeNumber;
      fBlockChangeNumber := 0;
      Inc(fNextChangeNumber);
      if fNextChangeNumber = 0 then
        Inc(fNextChangeNumber);
      if (fItems.Count > 0) and (PeekItem.ChangeNumber = iBlockID) and
        Assigned(OnAddedUndo) then
      begin
        OnAddedUndo( Self );
      end;
    end;
  end;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  if fItems.Count > fMaxUndoActions then begin
    fFullUndoImposible := True;
    while fItems.Count > fMaxUndoActions do begin
      Item := fItems[0];
      Item.Free;
      fItems.Delete(0);
    end;
  end;
end;

function TSynEditUndoList.GetCanUndo: boolean;
begin
  Result := fItems.Count > 0;
end;

function TSynEditUndoList.GetItemCount: integer;
begin
  Result := fItems.Count;
end;

procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
end;

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then
    Result := fItems[iLast];
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
    Result := fItems[iLast];
    fItems.Delete(iLast);
  end;
end;

procedure TSynEditUndoList.PushItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then begin
    fItems.Add(Item);
    EnsureMaxEntries;
    if (Item.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fMaxUndoActions then begin
    fMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
end;

function TSynEditUndoList.LastChangeReason: TSynChangeReason;
begin
  if fItems.Count = 0 then
    result := crNothing
  else
    result := TSynEditUndoItem(fItems[fItems.Count - 1]).fChangeReason;
end;

procedure TSynEditUndoList.AddGroupBreak;
var
  vDummy: TBufferCoord;
begin
  //Add the GroupBreak even if ItemCount = 0. Since items are stored in
  //reverse order in TCustomSynEdit.fRedoList, a GroupBreak could be lost.
  if LastChangeReason <> crGroupBreak then
  begin
    AddChange(crGroupBreak, vDummy, vDummy, '', smNormal);
  end;
end;

procedure TSynEditUndoList.SetInitialState(const Value: boolean);
begin
  if Value then
  begin
    if ItemCount = 0 then
      fInitialChangeNumber := 0
    else
      fInitialChangeNumber := PeekItem.ChangeNumber;
  end
  else
    if ItemCount = 0 then
    begin
      if fInitialChangeNumber = 0 then
        fInitialChangeNumber := -1;
    end
    else if PeekItem.ChangeNumber = fInitialChangeNumber then
      fInitialChangeNumber := -1;
end;

function TSynEditUndoList.GetInitialState: boolean;
begin
  if ItemCount = 0 then
    Result := fInitialChangeNumber = 0
  else
    Result := PeekItem.ChangeNumber = fInitialChangeNumber;
end;

function TSynEditUndoList.GetItems(Index: Integer): TSynEditUndoItem;
begin
  Result := TSynEditUndoItem(fItems[Index]);
end;

procedure TSynEditUndoList.SetItems(Index: Integer;
  const Value: TSynEditUndoItem);
begin
  fItems[Index] := Value;
end;

procedure TSynEditUndoList.DeleteItem(AIndex: Integer);
begin
  TSynEditUndoItem(fItems[AIndex]).Free;
  fItems.Delete(AIndex);
end;

end.



