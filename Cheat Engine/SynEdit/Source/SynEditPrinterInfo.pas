{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrinterInfo.pas, released 2000-06-01.

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

$Id: SynEditPrinterInfo.pas,v 1.5.2.1 2007/06/22 04:24:04 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Class retrieving info about selected printer and paper size.
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITPRINTERINFO}
unit SynEditPrinterInfo;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QPrinters;
{$ELSE}
  Windows,
  Printers;
{$ENDIF}

type
  //Printer info class - getting dimensions of paper
  TSynEditPrinterInfo = class
  private
    FPhysicalWidth: Integer;
    FPhysicalHeight: Integer;
    FPrintableWidth: Integer;
    FPrintableHeight: Integer;
    FLeftGutter: Integer;
    FRightGutter: Integer;
    FTopGutter: Integer;
    FBottomGutter: Integer;
    FXPixPrInch: Integer;
    FYPixPrInch: Integer;
    FXPixPrmm: Single;
    FYPixPrmm: Single;
    FIsUpdated: Boolean;
    procedure FillDefault;
    function GetBottomGutter: Integer;
    function GetLeftGutter: Integer;
    function GetPhysicalHeight: Integer;
    function GetPhysicalWidth: Integer;
    function GetPrintableHeight: Integer;
    function GetPrintableWidth: Integer;
    function GetRightGutter: Integer;
    function GetTopGutter: Integer;
    function GetXPixPrInch: Integer;
    function GetYPixPrInch: Integer;
    function GetXPixPrmm: Single;
    function GetYPixPrmm: Single;
  public
    procedure UpdatePrinter;
    function PixFromLeft(mmValue: Double): Integer;
    function PixFromRight(mmValue: Double): Integer;
    function PixFromTop(mmValue: Double): Integer;
    function PixFromBottom(mmValue: Double): Integer;
    property PhysicalWidth: Integer read GetPhysicalWidth;
    property PhysicalHeight: Integer read GetPhysicalHeight;
    property PrintableWidth: Integer read GetPrintableWidth;
    property PrintableHeight: Integer read GetPrintableHeight;
    property LeftGutter: Integer read GetLeftGutter;
    property RightGutter: Integer read GetRightGutter;
    property TopGutter: Integer read GetTopGutter;
    property BottomGutter: Integer read GetBottomGutter;
    property XPixPrInch: Integer read GetXPixPrInch;
    property YPixPrInch: Integer read GetYPixPrInch;
    property XPixPrmm: Single read GetXPixPrmm;
    property YPixPrmm: Single read GetYPixPrmm;
  end;

implementation

{ TSynEditPrinterInfo }

function TSynEditPrinterInfo.PixFromBottom(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FYPixPrmm - FBottomGutter);
end;

function TSynEditPrinterInfo.PixFromLeft(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FXPixPrmm - FLeftGutter);
end;

function TSynEditPrinterInfo.PixFromRight(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FXPixPrmm - FRightGutter);
end;

function TSynEditPrinterInfo.PixFromTop(mmValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(mmValue * FYPixPrmm - FTopGutter);
end;

procedure TSynEditPrinterInfo.FillDefault;
{In case of no printers installed this information is used
 (I think it's taken from a HP LaserJet III with A4 paper)}
begin
  FPhysicalWidth := 2481;
  FPhysicalHeight := 3507;
  FPrintableWidth := 2358;
  FPrintableHeight := 3407;
  FLeftGutter := 65;
  FRightGutter := 58;
  FTopGutter := 50;
  FBottomGutter := 50;
  FXPixPrInch := 300;
  FYPixPrInch := 300;
  FXPixPrmm := FXPixPrInch / 25.4;
  FYPixPrmm := FYPixPrInch / 25.4;
end;

function TSynEditPrinterInfo.GetBottomGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FBottomGutter;
end;

function TSynEditPrinterInfo.GetLeftGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FLeftGutter;
end;

function TSynEditPrinterInfo.GetPhysicalHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalHeight;
end;

function TSynEditPrinterInfo.GetPhysicalWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalWidth;
end;

function TSynEditPrinterInfo.GetPrintableHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableHeight;
end;

function TSynEditPrinterInfo.GetPrintableWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableWidth;
end;

function TSynEditPrinterInfo.GetRightGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FRightGutter;
end;

function TSynEditPrinterInfo.GetTopGutter: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FTopGutter;
end;

function TSynEditPrinterInfo.GetXPixPrInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPrInch;
end;

function TSynEditPrinterInfo.GetXPixPrmm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPrmm;
end;

function TSynEditPrinterInfo.GetYPixPrInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPrInch;
end;

function TSynEditPrinterInfo.GetYPixPrmm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPrmm;
end;

procedure TSynEditPrinterInfo.UpdatePrinter;
begin
  FIsUpdated := True;
{$IFNDEF SYN_CLX}
  Printer.Refresh;
{$ENDIF}
  if Printer.Printers.Count <= 0 then
  begin
    FillDefault;
    Exit;
  end;
  {************}
{$IFNDEF SYN_CLX}
  FPhysicalWidth := GetDeviceCaps(Printer.Handle, Windows.PhysicalWidth);
  FPhysicalHeight := GetDeviceCaps(Printer.Handle, Windows.PhysicalHeight);
{$ENDIF}
  FPrintableWidth := Printer.PageWidth; {or GetDeviceCaps(Printer.Handle, HorzRes);}
  FPrintableHeight := Printer.PageHeight; {or GetDeviceCaps(Printer.Handle, VertRes);}
  {************}
{$IFNDEF SYN_CLX}
  FLeftGutter := GetDeviceCaps(Printer.Handle, PhysicalOffsetX);
  FTopGutter := GetDeviceCaps(Printer.Handle, PhysicalOffsetY);
{$ENDIF}
  FRightGutter := FPhysicalWidth - FPrintableWidth - FLeftGutter;
  FBottomGutter := FPhysicalHeight - FPrintableHeight - FTopGutter;
  {************}
{$IFNDEF SYN_CLX}
  FXPixPrInch := GetDeviceCaps(Printer.Handle, LogPixelsX);
  FYPixPrInch := GetDeviceCaps(Printer.Handle, LogPixelsY);
{$ENDIF}
  FXPixPrmm := FXPixPrInch / 25.4;
  FYPixPrmm := FYPixPrInch / 25.4;
end;

end.

