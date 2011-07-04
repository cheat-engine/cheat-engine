{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditAutoComplete.pas, released 2000-06-25.

The Initial Author of the Original Code is Michael Hieke.
Portions written by Michael Hieke are Copyright 2000 Michael Hieke.
Portions written by Cyrille de Brebisson (from mwCompletionProposal.pas) are
Copyright 1999 Cyrille de Brebisson.
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

$Id: SynEditAutoComplete.pas,v 1.10 2004/05/06 19:16:44 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITAUTOCOMPLETE}
unit SynEditAutoComplete;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  {$IFDEF SYN_CLX}
  Qt,
  QMenus,
  Types,
  QSynEdit,
  QSynEditKeyCmds,
  {$ELSE}
  Windows,
  Menus,
  SynEdit,
  SynEditKeyCmds,
  {$ENDIF}
  Classes;

type
  TCustomSynAutoComplete = class(TComponent)
  protected
    fAutoCompleteList: TStrings;
    fCompletions: TStrings;
    fCompletionComments: TStrings;
    fCompletionValues: TStrings;
    fEditor: TCustomSynEdit;
    fEditors: TList;
    fEOTokenChars: string;
    fCaseSensitive: boolean;
    fParsed: boolean;
    procedure CompletionListChanged(Sender: TObject);
    function GetCompletions: TStrings;
    function GetCompletionComments: TStrings;
    function GetCompletionValues: TStrings;
    function GetEditorCount: integer;
    function GetNthEditor(Index: integer): TCustomSynEdit;
    procedure SetAutoCompleteList(Value: TStrings); virtual;
    procedure SetEditor(Value: TCustomSynEdit);
    procedure SynEditCommandHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
      Data: pointer; HandlerData: pointer);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddEditor(AEditor: TCustomSynEdit): boolean;
    function RemoveEditor(AEditor: TCustomSynEdit): boolean;

    procedure AddCompletion(const AToken, AValue, AComment: string);
    procedure Execute(AEditor: TCustomSynEdit); virtual;
    procedure ExecuteCompletion(const AToken: string; AEditor: TCustomSynEdit);
      virtual;
    procedure ParseCompletionList; virtual;
  public
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    property Completions: TStrings read GetCompletions;
    property CompletionComments: TStrings read GetCompletionComments;
    property CompletionValues: TStrings read GetCompletionValues;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property EditorCount: integer read GetEditorCount;
    property Editors[Index: integer]: TCustomSynEdit read GetNthEditor;
    property EndOfTokenChr: string read fEOTokenChars write fEOTokenChars;
  end;

  TSynAutoComplete = class(TCustomSynAutoComplete)
  published
    property AutoCompleteList;
    property CaseSensitive;
    property Editor;
    property EndOfTokenChr;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
{$ELSE}
  SynEditTypes,
{$ENDIF}
  SysUtils;

{ TCustomSynAutoComplete }

procedure TCustomSynAutoComplete.AddCompletion(const AToken, AValue, AComment: string);
begin
  if AToken <> '' then
  begin
    if (fAutoCompleteList.Count = 0) and (fCompletions.Count = 0) then
      fParsed := True;
    fCompletions.Add(AToken);
    fCompletionComments.Add(AComment);
    fCompletionValues.Add(AValue);
  end;
end;

function TCustomSynAutoComplete.AddEditor(AEditor: TCustomSynEdit): boolean;
var
  i: integer;
begin
  if AEditor <> nil then begin
    i := fEditors.IndexOf(AEditor);
    if i = -1 then begin
      AEditor.FreeNotification(Self);
      fEditors.Add(AEditor);
      AEditor.RegisterCommandHandler(SynEditCommandHandler, nil);
    end;
    Result := TRUE;
  end else
    Result := FALSE;
end;

procedure TCustomSynAutoComplete.CompletionListChanged(Sender: TObject);
begin
  fParsed := FALSE;
end;

constructor TCustomSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAutoCompleteList := TStringList.Create;
  TStringList(fAutoCompleteList).OnChange := CompletionListChanged;
  fCompletions := TStringList.Create;
  fCompletionComments := TStringList.Create;
  fCompletionValues := TStringList.Create;
  fEditors := TList.Create;
  fEOTokenChars := '()[]{}.';
end;

destructor TCustomSynAutoComplete.Destroy;
begin
  Editor := nil;
  while EditorCount > 0 do
    RemoveEditor( TCustomSynEdit(fEditors.Last) );

  inherited Destroy;
  fEditors.Free;
  fCompletions.Free;
  fCompletionComments.Free;
  fCompletionValues.Free;
  fAutoCompleteList.Free;
end;

procedure TCustomSynAutoComplete.Execute(AEditor: TCustomSynEdit);
var
  s: string;
  i, j: integer;
begin
  if AEditor <> nil then begin
    // get token
    s := AEditor.LineText;
    j := AEditor.CaretX;
    i := j - 1;
    if i <= Length(s) then begin
      while (i > 0) and (s[i] > ' ') and (Pos(s[i], fEOTokenChars) = 0) do
        Dec(i);
      Inc(i);
      s := Copy(s, i, j - i);
      ExecuteCompletion(s, AEditor);
    end;
  end;
end;

procedure TCustomSynAutoComplete.ExecuteCompletion(const AToken: string;
  AEditor: TCustomSynEdit);
var
  i, j, Len, IndentLen: integer;
  s: string;
  IdxMaybe, NumMaybe: integer;
  p: TBufferCoord;
  NewCaretPos: boolean;
  Temp: TStringList;
begin
  if not fParsed then
    ParseCompletionList;
  Len := Length(AToken);
  if (Len > 0) and (AEditor <> nil) and not AEditor.ReadOnly
    and (fCompletions.Count > 0)
  then begin
    // find completion for this token - not all chars necessary if unambiguous
    i := fCompletions.Count - 1;
    IdxMaybe := -1;
    NumMaybe := 0;
    if fCaseSensitive then begin
      while i > -1 do begin
        s := fCompletions[i];
        if AnsiCompareStr(s, AToken) = 0 then
          break
        else if AnsiCompareStr(Copy(s, 1, Len), AToken) = 0 then begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end else begin
      while i > -1 do begin
        s := fCompletions[i];
        if AnsiCompareText(s, AToken) = 0 then
          break
        else if AnsiCompareText(Copy(s, 1, Len), AToken) = 0 then begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end;
    if (i = -1) and (NumMaybe = 1) then
      i := IdxMaybe;
    if i > -1 then begin
      // select token in editor
      p := AEditor.CaretXY;
{begin}                                                                         //mh 2000-11-08
      AEditor.BeginUpdate;
      try
        AEditor.BlockBegin := BufferCoord(p.Char - Len, p.Line);
        AEditor.BlockEnd := p;
        // indent the completion string if necessary, determine the caret pos
        IndentLen := p.Char - Len - 1;
        p := AEditor.BlockBegin;
        NewCaretPos := FALSE;
        Temp := TStringList.Create;
        try
          Temp.Text := fCompletionValues[i];
          // indent lines
          if (IndentLen > 0) and (Temp.Count > 1) then begin
            s := StringOfChar(' ', IndentLen);
            for i := 1 to Temp.Count - 1 do
              Temp[i] := s + Temp[i];
          end;
          // find first '|' and use it as caret position
          for i := 0 to Temp.Count - 1 do begin
            s := Temp[i];
            j := Pos('|', s);
            if j > 0 then begin
              Delete(s, j, 1);
              Temp[i] := s;
//              if j > 1 then
//                Dec(j);
              NewCaretPos := TRUE;
              Inc(p.Line, i);
              if i = 0 then
//                Inc(p.x, j)
                Inc(p.Char, j - 1)
              else
                p.Char := j;
              break;
            end;
          end;
          s := Temp.Text;
          // strip the trailing #13#10 that was appended by the stringlist
          i := Length(s);
          if (i >= 2) and (s[i - 1] = #13) and (s[i] = #10) then
            SetLength(s, i - 2);
        finally
          Temp.Free;
        end;
        // replace the selected text and position the caret
        AEditor.SelText := s;
        if NewCaretPos then
          AEditor.CaretXY := p;
      finally
        AEditor.EndUpdate;                                                
      end;
{end}                                                                           //mh 2000-11-08
    end;
  end;
end;

function TCustomSynAutoComplete.GetCompletions: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletions;
end;

function TCustomSynAutoComplete.GetCompletionComments: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletionComments;
end;

function TCustomSynAutoComplete.GetCompletionValues: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletionValues;
end;

function TCustomSynAutoComplete.GetEditorCount: integer;
begin
  Result := fEditors.Count;
end;

function TCustomSynAutoComplete.GetNthEditor(Index: integer): TCustomSynEdit;
begin
  if (Index >= 0) and (Index < fEditors.Count) then
    Result := fEditors[Index]
  else
    Result := nil;
end;

procedure TCustomSynAutoComplete.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Editor then
      Editor := nil
    else if AComponent is TCustomSynEdit then
      RemoveEditor( TCustomSynEdit(AComponent) );
  end;
end;

procedure TCustomSynAutoComplete.ParseCompletionList;
var
  BorlandDCI: boolean;
  i, j, Len: integer;
  s, sCompl, sComment, sComplValue: string;

  procedure SaveEntry;
  begin
    fCompletions.Add(sCompl);
    sCompl := '';
    fCompletionComments.Add(sComment);
    sComment := '';
    fCompletionValues.Add(sComplValue);
    sComplValue := '';
  end;

begin
  fCompletions.Clear;
  fCompletionComments.Clear;
  fCompletionValues.Clear;

  if fAutoCompleteList.Count > 0 then begin
    s := fAutoCompleteList[0];
    BorlandDCI := (s <> '') and (s[1] = '[');

    sCompl := '';
    sComment := '';
    sComplValue := '';
    for i := 0 to fAutoCompleteList.Count - 1 do begin
      s := fAutoCompleteList[i];
      Len := Length(s);
      if BorlandDCI then begin
        // the style of the Delphi32.dci file
        if (Len > 0) and (s[1] = '[') then begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          j := 2;
          while (j <= Len) and (s[j] > ' ') do
            Inc(j);
          sCompl := Copy(s, 2, j - 2);
          // start of comment in DCI file
          while (j <= Len) and (s[j] <= ' ') do
            Inc(j);
          if (j <= Len) and (s[j] = '|') then
            Inc(j);
          while (j <= Len) and (s[j] <= ' ') do
            Inc(j);
          sComment := Copy(s, j, Len);
          if sComment[Length(sComment)] = ']' then
            SetLength(sComment, Length(sComment) - 1);
        end else begin
          if sComplValue <> '' then
            sComplValue := sComplValue + #13#10;
          sComplValue := sComplValue + s;
        end;
      end else begin
        // the original style
        if (Len > 0) and (s[1] <> '=') then begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          sCompl := s;
        end else if (Len > 0) and (s[1] = '=') then begin
          if sComplValue <> '' then
            sComplValue := sComplValue + #13#10;
          sComplValue := sComplValue + Copy(s, 2, Len);
        end;
      end;
    end;
    if sCompl <> '' then                                                        //mg 2000-11-07
      SaveEntry;
  end;
  fParsed := True;
end;

function TCustomSynAutoComplete.RemoveEditor(AEditor: TCustomSynEdit): boolean;
var
  i: integer;
begin
  if AEditor <> nil then
  begin
    i := fEditors.IndexOf(AEditor);
    if (i > -1) then
    begin
      if fEditor = AEditor then
        fEditor := nil;
      fEditors.Delete(i);
      AEditor.UnregisterCommandHandler(SynEditCommandHandler);
      {$IFDEF SYN_COMPILER_5_UP}
      RemoveFreeNotification( AEditor );
      {$ENDIF}
    end;
  end;
  Result := False;
end;

procedure TCustomSynAutoComplete.SetAutoCompleteList(Value: TStrings);
begin
  fAutoCompleteList.Assign(Value);
  fParsed := FALSE;
end;

procedure TCustomSynAutoComplete.SetEditor(Value: TCustomSynEdit);
begin
  if Value <> fEditor then
  begin
    if fEditor <> nil then
      RemoveEditor(fEditor);
    fEditor := Value;
    if (Value <> nil) then
      AddEditor(Value);
  end;
end;

procedure TCustomSynAutoComplete.SynEditCommandHandler(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer;
  HandlerData: pointer);
begin
  if not AfterProcessing and not Handled and (Command = ecAutoCompletion) then
  begin
    Handled := TRUE;
    Execute(Sender as TCustomSynEdit);
  end;
end;

end.

