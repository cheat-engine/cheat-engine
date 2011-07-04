{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
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

$Id: SynEditKbdHandler.pas,v 1.10 2004/01/04 21:49:04 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITKBDHANDLER}
unit SynEditKbdHandler;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Types,
  QGraphics,
  QControls,
  QForms,
  QSynEditTypes,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  SynEditTypes,
{$ENDIF}
  SysUtils,
  Classes;

type
  { This class provides a TWinControl-Object which supports only the
    needed Methods }
  TKeyboardControl = class (TWinControl)
    public
      property OnKeyDown;
      property OnKeyPress;
      property OnMouseDown;
  end;

  TMouseCursorEvent =  procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

  TMethodList = class
  private
    fData: TList;
    function GetItem(aIndex: integer): TMethod;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aHandler: TMethod);
    procedure Remove(aHandler: TMethod);
    property Items[aIndex: integer]: TMethod read GetItem; default;
    property Count: integer read GetCount;
  end;

  TSynEditKbdHandler = class (TObject)
    private
      fKeyPressChain   : TMethodList;
      fKeyDownChain    : TMethodList;
      fKeyUpChain      : TMethodList;
      fMouseDownChain  : TMethodList;
      fMouseUpChain    : TMethodList;
      fMouseCursorChain: TMethodList;
      { avoid infinite recursiveness }
      fInKeyPress      : Boolean;
      fInKeyDown       : Boolean;
      fInKeyUp         : Boolean;
      fInMouseDown     : Boolean;
      fInMouseUp       : Boolean;
      fInMouseCursor   : Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      procedure ExecuteKeyPress (Sender: TObject; var Key: Char);
      procedure ExecuteKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure ExecuteKeyUp (Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure ExecuteMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure ExecuteMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure ExecuteMouseCursor(Sender: TObject; const aLineCharPos: TBufferCoord;
        var aCursor: TCursor);

      procedure AddKeyDownHandler (aHandler : TKeyEvent);
      procedure RemoveKeyDownHandler (aHandler : TKeyEvent);
      procedure AddKeyUpHandler (aHandler : TKeyEvent);
      procedure RemoveKeyUpHandler (aHandler : TKeyEvent);
      procedure AddKeyPressHandler (aHandler : TKeyPressEvent);
      procedure RemoveKeyPressHandler (aHandler : TKeyPressEvent);
      procedure AddMouseDownHandler(aHandler: TMouseEvent);
      procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
      procedure AddMouseUpHandler(aHandler: TMouseEvent);
      procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
      procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
      procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
  end;


implementation

{ TSynEditKbdHandler }

procedure TSynEditKbdHandler.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  fKeyDownChain.Add( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKeyUpChain.Add( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.AddKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKeyPressChain.Add( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fMouseDownChain.Add( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fMouseUpChain.Add( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fMouseCursorChain.Add( TMethod(aHandler) );
end;

constructor TSynEditKbdHandler.Create;
  begin
    { Elements to handle KeyDown-Events }
    fKeyDownChain := TMethodList.Create;

    { Elements to handle KeyUp-Events }
    fKeyUpChain := TMethodList.Create;

    { Elements to handle KeyPress-Events }
    fKeyPressChain := TMethodList.Create;

    { Elements to handle MouseDown Events }
    fMouseDownChain := TMethodList.Create;

    { Elements to handle MouseUp Events }
    fMouseUpChain := TMethodList.Create;

    { Elements to handle MouseCursor Events }
    fMouseCursorChain := TMethodList.Create;
  end;

destructor TSynEditKbdHandler.Destroy;
  begin
    fKeyPressChain.Free;
    fKeyDownChain.Free;
    fKeyUpChain.Free;
    fMouseDownChain.Free;
    fMouseUpChain.Free;
    fMouseCursorChain.Free;

    inherited Destroy;
  end;

procedure TSynEditKbdHandler.ExecuteKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
  var
    idx : Integer;
  begin
    if fInKeyDown then
      exit;
    fInKeyDown := true;
    try
      with fKeyDownChain do begin
        for idx := Count - 1 downto 0 do begin
          TKeyEvent(Items[idx])(Sender,Key,Shift);
          if (Key = 0) then begin
            fInKeyDown := false;
            exit;
          end;
        end;
      end;
    finally
      fInKeyDown := false;
    end;
  end;

procedure TSynEditKbdHandler.ExecuteKeyUp (Sender: TObject; var Key: Word; Shift: TShiftState);
  var
    idx : Integer;
  begin
    if fInKeyUp then
      exit;
    fInKeyUp := true;
    try
      with fKeyUpChain do begin
        for idx := Count - 1 downto 0 do begin
          TKeyEvent(Items[idx])(Sender,Key,Shift);
          if (Key = 0) then begin
            fInKeyUp := false;
            exit;
          end;
        end;
      end;
    finally
      fInKeyUp := false;
    end;
  end;

procedure TSynEditKbdHandler.ExecuteKeyPress (Sender: TObject; var Key: Char);
  var
    idx : Integer;
  begin
    if fInKeyPress then
      exit;
    fInKeyPress := true;
    try
      with fKeyPressChain do begin
        for idx := Count - 1 downto 0 do begin
          TKeyPressEvent(Items[idx])(Sender,Key);
          if (Key = #0) then begin
            fInKeyPress := false;
            exit;
          end;
        end;
      end;
    finally
      fInKeyPress := false;
    end;
  end;

procedure TSynEditKbdHandler.ExecuteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cHandler: Integer;
begin
  if fInMouseDown then
    Exit;
  fInMouseDown := True;
  try
    for cHandler := fMouseDownChain.Count - 1 downto 0 do
      TMouseEvent(fMouseDownChain[cHandler])( Sender, Button, Shift, X, Y );
  finally
    fInMouseDown := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cHandler: Integer;
begin
  if fInMouseUp then
    Exit;
  fInMouseUp := True;
  try
    for cHandler := fMouseUpChain.Count - 1 downto 0 do
      TMouseEvent(fMouseUpChain[cHandler])( Sender, Button, Shift, X, Y );
  finally
    fInMouseUp := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteMouseCursor(Sender: TObject;
  const aLineCharPos: TBufferCoord; var aCursor: TCursor);
var
  cHandler: Integer;
begin
  if fInMouseCursor then
    Exit;
  fInMouseCursor := True;
  try
    for cHandler := fMouseCursorChain.Count - 1 downto 0 do
      TMouseCursorEvent(fMouseCursorChain[cHandler])(Sender, aLineCharPos, aCursor);
  finally
    fInMouseCursor := False;
  end;
end;

procedure TSynEditKbdHandler.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  fKeyDownChain.Remove( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKeyUpChain.Remove( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.RemoveKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKeyPressChain.Remove( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fMouseDownChain.Remove( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fMouseUpChain.Remove( TMethod(aHandler) );
end;

procedure TSynEditKbdHandler.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fMouseCursorChain.Remove( TMethod(aHandler) );
end;

{ TMethodList }

procedure TMethodList.Add(aHandler: TMethod);
begin
  fData.Add( aHandler.Data );
  fData.Add( aHandler.Code );
end;

constructor TMethodList.Create;
begin
  fData := TList.Create;
end;

destructor TMethodList.Destroy;
begin
  fData.Free;
end;

function TMethodList.GetCount: integer;
begin
  Result := fData.Count div 2;
end;

function TMethodList.GetItem(aIndex: integer): TMethod;
begin
  aIndex := aIndex * 2;
  Result.Data := fData[aIndex];
  Result.Code := fData[aIndex+1];
end;

procedure TMethodList.Remove(aHandler: TMethod);
var
  cPos: integer;
begin
  cPos := fData.Count -2;
  while cPos >= 0 do
  begin
    if (fData.List[cPos] = aHandler.Data) and (fData.List[cPos +1] = aHandler.Code) then
    begin
      fData.Delete( cPos );
      fData.Delete( cPos );
      Exit;
    end;
    Dec( cPos, 2 );
  end;
end;

end.
