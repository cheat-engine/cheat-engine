{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintTypes.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPrintTypes.pas,v 1.5 2004/10/09 12:54:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  Wrapping across page boundaries is not supported
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Misc types and procedures used in printing and previewing
-------------------------------------------------------------------------------}


{$IFNDEF QSYNEDITPRINTTYPES}
unit SynEditPrintTypes;
{$ENDIF}

interface

uses
  Classes, SysUtils;

const
  DefLeft = 25; //Default left margin [mm]
  DefRight = 15; //Default right margin [mm]
  DefTop = 25; //Default top margin [mm]
  DefBottom = 25; //Default bottom margin [mm]
  DefHeader = 15; //Default margin from top of paper to bottom of header [mm]
  DefFooter = 15; //Default margin from top of footer to bottom of paper [mm]
  DefLeftHFTextIndent = 2; //Default Header/footer indent from left margin [mm]
  DefRightHFTextIndent = 2; //Default Header/footer indent from right margin [mm]
  DefHFInternalMargin = 0.5; //Default Internal margin between Header/footer text and lines [mm]
  DefGutter = 0; //Default Binding gutter - added to left or right margin [mm]
type
//Frame around header/footer
  TFrameType = (ftLine, ftBox, ftShaded);
  TFrameTypes = set of TFrameType;
//Margin units (internally is allways used [mm])
  TUnitSystem = (usMM, usCM, usInch, muThousandthsOfInches);
//Print status events
  TSynPrintStatus = (psBegin, psNewPage, psEnd);
  TPrintStatusEvent = procedure(Sender: TObject; Status: TSynPrintStatus;
    PageNumber: Integer; var Abort: Boolean) of object;
//Event raised when a line is printed (can be used to generate Table of Contents)
  TPrintLineEvent = procedure(Sender: TObject; LineNumber, PageNumber: Integer) of object;
  TSysCharSet = set of Char;
type
  TWrapPos = class
  public
    Index: Integer;
  end;

function IntToRoman(Value: Integer): string;

function WrapTextEx(const Line: string; BreakChars: TSysCharSet;
  MaxCol: Integer; AList: TList): Boolean;

implementation

//Returns wrapping positions in AList.
function WrapTextEx(const Line: string; BreakChars: TSysCharSet;
  MaxCol: Integer; AList: TList): Boolean;
var
  WrapPos: TWrapPos;
  Pos, PreviousPos: Integer;
  Found: Boolean;
begin
  if Length(Line) <= MaxCol then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  Pos := 1;
  PreviousPos := 0;
  WrapPos := TWrapPos.Create;
  while Pos <= Length(Line) do
  begin
    Found := (Pos - PreviousPos > MaxCol) and (WrapPos.Index <> 0);
    if not Found and (Line[Pos] in BreakChars) then // We found a possible break
      WrapPos.Index := Pos;

    if Found then
    begin
      Result := True;
      AList.Add(WrapPos);
      PreviousPos := WrapPos.Index;

      // If more wraps needed and not end of line then a new wrap is created
      if ((Length(Line) - PreviousPos) > MaxCol) and (Pos < Length(Line)) then
        WrapPos := TWrapPos.Create
      else
        Break;
    end;
    Pos := Pos + 1;
  end;

  if (AList.Count = 0) or (AList.Last <> WrapPos) then
    WrapPos.Free;
end;

//Integer to Roman - copied from SWAG
function IntToRoman(Value: Integer): string;
begin
  Result := '';
  while Value >= 1000 do begin
    Result := Result + 'M';
    Value := Value - 1000;
  end;

  if Value >= 900 then
  begin
    Result := Result + 'CM';
    Value := Value - 900;
  end;

  while Value >= 500 do
  begin
    Result := Result + 'D';
    Value := Value - 500;
  end;

  if Value >= 400 then
  begin
    Result := Result + 'CD';
    Value := Value - 400;
  end;

  while Value >= 100 do
  begin
    Result := Result + 'C';
    Value := Value - 100;
  end;

  if Value >= 90 then
  begin
    Result := Result + 'XC';
    Value := Value - 90;
  end;

  while Value >= 50 do
  begin
    Result := Result + 'L';
    Value := Value - 50;
  end;

  if Value >= 40 then
  begin
    Result := Result + 'XL';
    Value := Value - 40;
  end;

  while Value >= 10 do
  begin
    Result := Result + 'X';
    Value := Value - 10;
  end;

  if Value >= 9 then
  begin
    Result := Result + 'IX';
    Value := Value - 9;
  end;

  while Value >= 5 do
  begin
    Result := Result + 'V';
    Value := Value - 5;
  end;

  if Value >= 4 then
  begin
    Result := Result + 'IV';
    Value := Value - 4;
  end;

  while Value > 0 do
  begin
    Result := Result + 'I';
    DEC(Value);
  end;
end;

end.

