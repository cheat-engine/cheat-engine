{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynAutoCorrect.pas, released 2001-10-05.
Author of this file is Aaron Chan. All Rights Reserved.

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

$Id: SynAutoCorrect.pas,v 1.13 2004/05/06 19:16:42 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{*******************************************************}
{                                                       }
{       Aerodynamica Components                         }
{       SynAutoCorrect 2.x                              }
{                                                       }
{       Copyright (c) 1996-2003, Aerodynamica Software  }
{                                                       }
{       Author: Aaron Chan                              }
{       Portions by Greg Nixon and Jan Fiala            }
{                                                       }
{*******************************************************}

{
  @author: Aaron Chan
  @url: http://aerodynamica.idz.net
  @comp-url: http://aerodynamica.idz.net/products.asp?id=SynAC_2
  @email: aerodynamica@idz.net
  @last-updated: 12/04/03
  @history:

    ! comment     * changed     + added
    - removed     @ bug-fixed   # todo

    12/04/2003
      - removed integrated sound support.
      * changed keyboard and mouse handling to use SynEdit plugin system.

    11/04/04 - Release 2.21:
      @ Fixed support for correction after delimiters.
      * SOUND_SUPPORT undefined by default.

    24/03/03 - Release 2.2:
      @ Fixed "Stack Overflow" bug and memory leak (fixed by Danail Traichev).

    30/09/02 - Release 2.1:
      @ Fixed bug when user KeyDown and MouseDown events weren't fired.
      + Added INI_FILES and REGISTRY compiler defines (to minimize code size
        if you don't need these features).
      * Further tidy-up of code.
      * Quite a few minor bug-fixes and tweaks.
      * Items editor enhanced.
      * Improved demo.
      * Registry and INI file entries are saved in a new and improved way, which
        overcomes some limitations set by the old method. If you still want to
        use the old method, define OLD_SAVE_METHOD.

    31/07/02 - Revision 2.01:
      @ Fixed bug which occured when undefining SOUND_SUPPORT (reported by
        Stefan Ascher).

    30/07/02 - First public release of version 2.0:
      @ MANY bugs fixed and small tweaks everywhere in the code (some
        courtesy of Jan Fiala).
      + Ability to play an optional WAVE file (or beep) on correction.
      + Options set.
      * New demo.
}

{$IFNDEF QSYNAUTOCORRECT}
unit SynAutoCorrect;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_WIN32} //Borland translation of Qt doesn't include Char handling
  Windows,
{$ELSE}
  Libc,
{$ENDIF}
{$IFDEF SYN_CLX}
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  Types,
  QSynEditMiscProcs,
  QSynEditTypes,
  QSynEditKeyCmds,
  QSynEdit,
{$ELSE}
  Registry,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  SynEditMiscProcs,
  SynEditTypes,
  SynEditKeyCmds,
  SynEdit,
  SynEditMiscClasses,   //TBetterRegistry
{$ENDIF}
  Classes,
  SysUtils,
  IniFiles;

type
  TAsSynAutoCorrectOption = (ascoCorrectOnMouseDown, ascoIgnoreCase,
    ascoMaintainCase);
  TAsSynAutoCorrectOptions = set of TAsSynAutoCorrectOption;

  TAutoCorrectAction = (aaCorrect, aaAbort);
  TAutoCorrectEvent = procedure(Sender: TObject;
    const AOriginal, ACorrection: string; Line, Column: Integer;
    var Action: TAutoCorrectAction) of object;

  TCustomSynAutoCorrect = class(TComponent)
  private
    { Private declarations }

    { Published properties and events. }
    FEditor: TCustomSynEdit;
    FEnabled: Boolean;
    FItems: TStrings;
    FItemSepChar: Char;
    FOptions: TAsSynAutoCorrectOptions;

    FOnAutoCorrect: TAutoCorrectEvent;
    FOnCorrected: TNotifyEvent;

    { Private variables and methods. }
    FPrevLine: Integer;

    function CorrectItemStart(EditLine, SearchString: string; StartPos: LongInt;
      MatchCase, WholeWord: Boolean): LongInt;
    function FindAndCorrect(var EditLine: string; Original, Correction: string;
      var CurrentX: Integer): Boolean;
    function PreviousToken: string;

    { Accessor methods. }
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
  protected
    { Protected declarations }
    procedure KeyboardHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
      Data: pointer; HandlerData: pointer); virtual;
    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetEditor(Value: TCustomSynEdit);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(AOriginal, ACorrection: string);
    function AutoCorrectAll: Boolean;
    procedure Delete(AIndex: Integer);
    procedure Edit(AIndex: Integer; ANewOriginal, ANewCorrection: string);

    procedure LoadFromINI(AFileName, ASection: string);
    procedure SaveToINI(AFileName, ASection: string);

{$IFNDEF SYN_CLX}
    procedure LoadFromRegistry(ARoot: DWORD; AKey: string);
    procedure SaveToRegistry(ARoot: DWORD; AKey: string);
{$ENDIF}

    function LoadFromList(AFileName: string): Boolean;
    procedure SaveToList(AFileName: string);

    { Utility functions. }
    function HalfString(Str: string; GetFirstHalf: Boolean): string;
  public
    { Published declarations }
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property Items: TStrings read GetItems write SetItems;
    property ItemSepChar: Char read FItemSepChar write FItemSepChar default #9;
    property Options: TAsSynAutoCorrectOptions read FOptions write FOptions
      default [ascoIgnoreCase, ascoMaintainCase];

    property OnAutoCorrect: TAutoCorrectEvent read FOnAutoCorrect
      write FOnAutoCorrect;
    property OnCorrected: TNotifyEvent read FOnCorrected write FOnCorrected;
  end;

  TSynAutoCorrect = class(TCustomSynAutoCorrect)
  published
    { Published declarations }
    property Enabled;
    property Editor;
    property Items;
    property ItemSepChar;
    property Options;

    property OnAutoCorrect;
    property OnCorrected;
  end;

var
  AC_IdentChars: set of char;

implementation

const
  FStrSepChar = '|';

{ TCustomSynAutoCorrect }

constructor TCustomSynAutoCorrect.Create(AOwner: TComponent);
var
  i: char;

begin
  inherited Create(AOwner);

{$IFDEF SYN_WIN32}
  for i := #33 to #255 do if IsCharAlphaNumeric(i) then Include(AC_IdentChars, i);
{$ELSE}
  for i := #33 to #255 do if isalpha(Ord(i)) <> 0 then Include(AC_IdentChars, i);
{$ENDIF}

  FEnabled := True;
  FItems := TStringList.Create;
  FItemSepChar := #9;
  FOptions := [ascoIgnoreCase, ascoMaintainCase];
  FPrevLine := -1;
//  FEditor := nil; initialized by Delphi
end;

destructor TCustomSynAutoCorrect.Destroy;
begin
  Editor := nil;
  inherited;
  FItems.Free;
end;

{ Utilities. }
function TCustomSynAutoCorrect.HalfString(Str: string;
  GetFirstHalf: Boolean): string;
var
  i: Integer;

begin
  i := LastDelimiter(FItemSepChar, Str);
  if i = 0 then i := Pred(MaxInt);

  if GetFirstHalf then
    Result := Copy(Str, 1, Pred(i))
  else
    Result := Copy(Str, Succ(i), MaxInt);
end;

procedure TCustomSynAutoCorrect.LoadFromIni(AFileName, ASection: string);
var
  i: Integer;
  Original, Correction: string;
  Reg: TIniFile;

begin
  Reg := TIniFile.Create(AFileName);
  try
    FItems.Clear;
    with Reg do
      for i := 0 to Pred(ReadInteger(ASection, 'Count', 0)) do
      begin
        Original := ReadString(ASection, 'Original' + IntToStr(i), '');
        Correction := ReadString(ASection, 'Correction' + IntToStr(i), '');
        if not ((Original = '') and (Correction = '')) then
          FItems.Add(Original + FItemSepChar + Correction);
      end;
  finally
    Reg.Free;
  end;
end;

procedure TCustomSynAutoCorrect.SaveToIni(AFileName, ASection: string);
var
  i: Integer;
  Reg: TIniFile;

begin
  Reg := TIniFile.Create(AFileName);
  try
    with Reg do
    begin
      WriteInteger(ASection, 'Count', FItems.Count);
      for i := 0 to Pred(FItems.Count) do
      begin
        WriteString(ASection, 'Original' + IntToStr(i),
          HalfString(FItems[i], True));
        WriteString(ASection, 'Correction' + IntToStr(i),
          HalfString(FItems[i], False));
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TCustomSynAutoCorrect.LoadFromList(AFileName: string): Boolean;
begin
  Result := False;
  if FileExists(AFileName) then
  begin
    FItems.LoadFromFile(AFileName);
    Result := True;
  end;
end;

procedure TCustomSynAutoCorrect.SaveToList(AFileName: string);
begin
  FItems.SaveToFile(AFileName);
end;

{$IFNDEF SYN_CLX}
procedure TCustomSynAutoCorrect.LoadFromRegistry(ARoot: DWORD; AKey: string);
var
  i: Integer;
  Original, Correction: string;
  Reg: TRegIniFile;

begin
  Reg := TRegIniFile.Create('');
  try
    with Reg do
    begin
      RootKey := ARoot;
      TBetterRegistry(Reg).OpenKeyReadOnly(AKey);
      FItems.Clear;
      for i := 0 to Pred(ReadInteger('', 'Count', 0)) do
      begin
        Original := ReadString('', 'Original' + IntToStr(i), '');
        Correction := ReadString('', 'Correction' + IntToStr(i), '');
        if not ((Original = '') and (Correction = '')) then
          FItems.Add(Original + FItemSepChar + Correction);
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TCustomSynAutoCorrect.SaveToRegistry(ARoot: DWORD; AKey: string);
var
  i: Integer;
  Reg: TRegIniFile;

begin
  Reg := TRegIniFile.Create('');
  try
    with Reg do
    begin
      RootKey := ARoot;
      OpenKey(AKey, True);
      WriteInteger('', 'Count', FItems.Count);
      for i := 0 to Pred(FItems.Count) do
      begin
        WriteString('', 'Original' + IntToStr(i), HalfString(FItems[i], True));
        WriteString('', 'Correction' + IntToStr(i),
          HalfString(FItems[i], False));
      end;
    end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

procedure TCustomSynAutoCorrect.Add(AOriginal, ACorrection: string);
begin
  FItems.Add(AOriginal + FItemSepChar + ACorrection);
end;

function TCustomSynAutoCorrect.AutoCorrectAll: Boolean;
var
  i, cx: Integer;
  s, Original, Correction, CurrText: string;

begin
  Result := False;
  if Assigned(Editor) then
  begin
    s := Editor.Lines.Text;
    cx := -1;

    for i := 0 to Pred(FItems.Count) do
    begin
      CurrText := FItems[i];
      Original := HalfString(CurrText, True);
      Correction := HalfString(CurrText, False);
      FindAndCorrect(s, Original, Correction, cx);
    end;
    Editor.Lines.Text := s;
  end;
end;

function TCustomSynAutoCorrect.CorrectItemStart(EditLine, SearchString: string;
  StartPos: LongInt; MatchCase, WholeWord: Boolean): LongInt;
var
  SearchCount, I: Integer;
  C: Char;
  CharMap: array [Char] of Char;
  CurBuf, Buf: PChar;
  BufLen: Integer;

const
  WordDelimiters: set of Char = [#0..#32];

  function FindNextWordStart(var BufPtr: PChar): Boolean;
  begin
    while (SearchCount > 0) and not (BufPtr^ in WordDelimiters) do
    begin
      Inc(BufPtr, 1);
      Dec(SearchCount);
    end;

    while (SearchCount > 0) and (BufPtr^ in WordDelimiters) do
    begin
      Inc(BufPtr, 1);
      Dec(SearchCount);
    end;

    Result := SearchCount >= 0;  
  end;

  function ScanText(var BufPtr: PChar): Boolean;
  var
     FirstWord: Boolean;
  begin
    Result := False;

    FirstWord := True;

    if WholeWord then
    begin
       while (SearchCount > 0) and (BufPtr^ in WordDelimiters) do
       begin
         Inc(BufPtr, 1);
         Dec(SearchCount);
       end;
    end;

    while SearchCount >= 0 do
    begin
      if WholeWord and (FirstWord = False) then
        if not FindNextWordStart(BufPtr) then Break;
      I := 0;
      while (CharMap[BufPtr[I]] = SearchString[I+1]) do
      begin
        Inc(I);
        if I >= Length(SearchString) then
        begin
          if (not WholeWord) or (SearchCount = 0) or
            (BufPtr[I] in WordDelimiters) then
          begin
            Result := True;
            Exit;
          end;
          Break;
        end;
      end;
      FirstWord := False;
      Inc(BufPtr);
      Dec(SearchCount);
    end;
  end;

begin
  Result := -1;
  BufLen := Length(EditLine);
  Buf := PChar(EditLine);

  if BufLen > 0 then
  begin
    SearchCount := succ(BufLen - StartPos - Length(SearchString));

    if (SearchCount >= 0) and (SearchCount <= BufLen) and
      (StartPos + SearchCount <= BufLen) then
    begin
      CurBuf := PChar( @Buf[StartPos] );
      for C := Low(CharMap) to High(CharMap) do
      {$IFDEF SYN_CLX}
      begin
        if not MatchCase then
          CharMap[C] := UpCase(C)
        else
          CharMap[C] := C;
      end;
      {$ELSE}
        CharMap[C] := C;
      {$ENDIF}
      if not MatchCase then
      begin
      {$IFDEF SYN_CLX}
        SearchString := UpperCase(SearchString);
      {$ELSE}
        CharUpperBuff(PChar(@CharMap), SizeOf(CharMap));
        CharUpperBuff(@SearchString[1], Length(SearchString));
      {$ENDIF}
      end;
      if not ScanText(CurBuf) then
        CurBuf := nil
      else
      begin
        if CurBuf <> nil then
          Result := CurBuf - Buf;
      end;
    end;
  end;

  CurBuf := nil;
end;

procedure TCustomSynAutoCorrect.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

procedure TCustomSynAutoCorrect.Edit(AIndex: Integer;
  ANewOriginal, ANewCorrection: string);
begin
  if AIndex > -1 then
    FItems[AIndex] := ANewOriginal + FItemSepChar + ANewCorrection;
end;

procedure TCustomSynAutoCorrect.KeyboardHandler(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
  Data: pointer; HandlerData: pointer);
var
  b: Boolean;
  i, cx: Integer;
  s, Original, Correction, CurrText: string;

begin
  if Enabled and (not AfterProcessing) and (not Handled) then
  begin
    FPrevLine := Editor.CaretY;
    case Command of
      ecLineBreak, ecTab, ecChar:
      begin
        if (Command = ecChar) and (AChar in AC_IdentChars) then
          Exit;
        b := False;
        s := PreviousToken;
        if s <> '' then
        begin
          cx := Editor.CaretX;
          for i := 0 to Pred(FItems.Count) do
          begin
            CurrText := FItems[i];
            Original := HalfString(CurrText, True);
            Correction := HalfString(CurrText, False);
            b := b or FindAndCorrect(s, Original, Correction, cx);
          end;
          
          if Assigned(OnCorrected) then
            OnCorrected( Self );
        end;
      end;
    end; {endcase}
  end;
end;

procedure TCustomSynAutoCorrect.MouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Action: TAutoCorrectAction;
  b: Boolean;
  i, cx: Integer;
  s, Original, Correction, CurrText: string;

begin
  if ascoCorrectOnMouseDown in FOptions then
  begin
    if Assigned(Editor) and Enabled and (FPrevLine <> -1) then
    begin
      b := False;
      s := Editor.Lines[Pred(FPrevLine)];
      cx := -1;

      for i := 0 to Pred(FItems.Count) do
      begin
        CurrText := FItems[i];
        Original := HalfString(CurrText, True);
        Correction := HalfString(CurrText, False);
        b := b or FindAndCorrect(s, Original, Correction, cx);
      end;

      if b then
      begin
        if Assigned(FOnAutoCorrect) then
        begin
          Action := aaCorrect;
          FOnAutoCorrect(Self, Editor.Lines[Pred(FPrevLine)], s, Editor.CaretY,
            0, Action);
          if Action = aaAbort then Exit;
        end;
        Editor.Lines[Pred(FPrevLine)] := s;
        
        if Assigned(OnCorrected) then
          OnCorrected( Self );
      end;
    end;
  end;
end;

function TCustomSynAutoCorrect.FindAndCorrect(var EditLine: string;
  Original, Correction: string; var CurrentX: Integer): Boolean;
var
  StartPos: LongInt;
  EndPos: Integer;
  FoundText, ReplaceDefText: string;
  p: TBufferCoord;
  Action: TAutoCorrectAction;

  function FirstCapCase(S: string): string;
  begin
    if S <> '' then
    begin
      s := LowerCase(s);
      s[1] := UpCase(s[1]);
    end;

    Result := s;
  end;

begin
  Result := False;
  ReplaceDefText := Correction;
  StartPos := 0;
  EndPos := Length(Original);

  if (Editor <> nil) and not (Editor.ReadOnly) then
  begin
    StartPos := CorrectItemStart(EditLine, Original, StartPos,
      not (ascoIgnoreCase in FOptions), True);

    while StartPos > -1 do
    begin
      if (ascoMaintainCase in FOptions) then
      begin
        Correction := ReplaceDefText;
        FoundText := Copy(EditLine,StartPos+1,EndPos);

        if FoundText = AnsiUpperCase(FoundText) then
          Correction := AnsiUpperCase(Correction)
        else
        begin
          if FoundText = AnsiLowerCase(FoundText) then
            Correction := AnsiLowerCase(Correction)
          else
          begin
            if FoundText = FirstCapCase(FoundText) then
              Correction := FirstCapCase(Correction);
          end;
        end;
      end;

      if CurrentX > - 1 then
      begin
        p := Editor.CaretXY;
        if Assigned(FOnAutoCorrect) then
        begin
          Action := aaCorrect;
          FOnAutoCorrect(Self, Original, Correction, P.Line, P.Char, Action);

          if Action = aaAbort then Break;
        end;

        Editor.BeginUpdate;

        try
          if p.Char = 0 then
            Editor.BlockBegin := BufferCoord(p.Char - 1 - EndPos, p.Line)
          else
            Editor.BlockBegin := BufferCoord(p.Char - EndPos, p.Line);

          Editor.BlockEnd := p;
          p := Editor.BlockBegin;
          Editor.SelText := Correction;
          Result := True;
        finally
          Editor.EndUpdate;
        end;

        Break;
      end
      else
      begin
        Result := True;
        EditLine := Copy(EditLine, 1, StartPos) + Correction +
          Copy(EditLine, StartPos + EndPos + 1, MaxInt);
        Inc(StartPos, EndPos);
        StartPos := CorrectItemStart(EditLine, Original, StartPos,
          not (ascoIgnoreCase in FOptions), True);
      end;
    end;
  end;
end;
                      
function TCustomSynAutoCorrect.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TCustomSynAutoCorrect.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FEditor) then
  begin
    Editor := nil;
  end;
end;

function TCustomSynAutoCorrect.PreviousToken: string;
var
  i, cx: Integer;

begin
  Result := Editor.LineText;
  cx := Editor.CaretX;
  i := Pred(cx);

  if i <= Length(Result) then
  begin
    while (i > 0) and (Result[i] > ' ') do Dec(i);
    Inc(i);
    Result := Copy(Result, i, cx - i);
  end
  else
    Result := '';
end;

procedure TCustomSynAutoCorrect.SetEditor(Value: TCustomSynEdit);
begin
  if FEditor <> Value then
  begin
    if Assigned(FEditor) then
    begin
      Editor.RemoveMouseDownHandler( MouseDownHandler );
      Editor.UnregisterCommandHandler( KeyboardHandler );
{$IFDEF SYN_COMPILER_5_UP}
      Editor.RemoveFreeNotification(Self);
{$ENDIF}
    end;

    FEditor := Value;

    if Assigned(FEditor) then
    begin
      Editor.FreeNotification(Self);
      Editor.RegisterCommandHandler( KeyboardHandler, nil );
      Editor.AddMouseDownHandler( MouseDownHandler );
    end;
  end;
end;

procedure TCustomSynAutoCorrect.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

end.
