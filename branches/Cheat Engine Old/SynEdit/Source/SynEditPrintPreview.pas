{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintPreview.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
Portions written by Michael Hieke are copyright 2000 Michael Hieke.
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

$Id: SynEditPrintPreview.pas,v 1.18 2003/12/27 22:18:43 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Print preview component. Allmost identical to code developed by Michael Hieke.
  It is important to call UpdatePreview whenever things change (i.e. just
  before the preview is shown, and when the printer is changed)
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITPRINTPREVIEW}
unit SynEditPrintPreview;
{$ENDIF}

{$I SynEdit.inc}

{$M+}
interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QControls,
  QGraphics,
  QForms,
  Types,
  QSynEditPrint,
{$ELSE}
  {$IFDEF SYN_COMPILER_7}
  Themes,
  {$ENDIF}
  Windows,
  Controls,
  Messages,
  Graphics,
  Forms,
  SynEditPrint,
{$ENDIF}
  Classes,
  SysUtils;

type
//Event raised when page is changed in preview
  TPreviewPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TSynPreviewScale = (pscWholePage, pscPageWidth, pscUserScaled);

  {$IFNDEF SYN_COMPILER_4_UP}
  TWMMouseWheel = record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;
  {$ENDIF}

  TSynEditPrintPreview = class(TCustomControl)
  protected
    FBorderStyle: TBorderStyle;
    FSynEditPrint: TSynEditPrint;
    FScaleMode: TSynPreviewScale;
    FScalePercent: Integer;
        // these are in pixels ( = screen device units)
    FVirtualSize: TPoint;
    FVirtualOffset: TPoint;
    FPageSize: TPoint;
    FScrollPosition: TPoint;
    FPageBG: TColor;
    FPageNumber: Integer;
    FShowScrollHint: Boolean;
    FOnPreviewPage: TPreviewPageEvent;
    FOnScaleChange: TNotifyEvent;                                               // JD 2002-01-9
  {$IFNDEF SYN_CLX}
    FWheelAccumulator: Integer;
  {$ENDIF}
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetPageBG(Value: TColor);
    procedure SetSynEditPrint(Value: TSynEditPrint);
    procedure SetScaleMode(Value: TSynPreviewScale);
    procedure SetScalePercent(Value: Integer);
  private
  {$IFNDEF SYN_CLX}
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message
      {$IFDEF SYN_COMPILER_3_UP} WM_MOUSEWHEEL {$ELSE} $020A {$ENDIF};
  {$ENDIF}
    procedure PaintPaper;
    function GetPageCount: Integer;
  protected
  {$IFNDEF SYN_CLX}
    procedure CreateParams(var Params: TCreateParams); override;
  {$ENDIF}
    function GetPageHeightFromWidth(AWidth: Integer): Integer;
    function GetPageHeight100Percent: Integer;
    function GetPageWidthFromHeight(AHeight: Integer): Integer;
    function GetPageWidth100Percent: Integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ScrollHorzFor(Value: Integer);
    procedure ScrollHorzTo(Value: Integer); virtual;
    procedure ScrollVertFor(Value: Integer);
    procedure ScrollVertTo(Value: Integer); virtual;
    procedure UpdateScrollbars; virtual;
    procedure SizeChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure UpdatePreview;
    procedure NextPage;
    procedure PreviousPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Print;
    property PageNumber: Integer read FPageNumber;
    property PageCount: Integer read GetPageCount;
  published
    property Align default alClient;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property Color default clAppWorkspace;
    property Cursor;
    property PageBGColor: TColor read FPageBG write SetPageBG default clWhite;
    property PopupMenu;                                                         // JD 2002-01-9
    property SynEditPrint: TSynEditPrint read FSynEditPrint
      write SetSynEditPrint;
    property ScaleMode: TSynPreviewScale read FScaleMode write SetScaleMode
      default pscUserScaled;
    property ScalePercent: Integer read FScalePercent write SetScalePercent
      default 100;
    property Visible default True;
    property ShowScrollHint: Boolean read FShowScrollHint write FShowScrollHint
      default True;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnPreviewPage: TPreviewPageEvent read FOnPreviewPage
      write FOnPreviewPage;
    property OnScaleChange: TNotifyEvent read FOnScaleChange                    // JD 2002-01-9
      write FOnScaleChange;                                                     // JD 2002-01-9
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  MARGIN_X = 12; // margin width left and right of page
  MARGIN_Y = 12; // margin height above and below page
  SHADOW_SIZE = 2; // page shadow width

{ TSynEditPrintPreview }

constructor TSynEditPrintPreview.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF SYN_COMPILER_7_UP}
  {$IFNDEF SYN_CLX}
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  {$ENDIF}
{$ENDIF}
  FBorderStyle := bsSingle;
  FScaleMode := pscUserScaled;
  FScalePercent := 100;
  FPageBG := clWhite;
  Width := 200;
  Height := 120;
  ParentColor := False;
  Color := clAppWorkspace;
  Visible := True;
  FPageNumber := 1;
  FShowScrollHint := True;
  Align := alClient;
{$IFNDEF SYN_CLX}
  FWheelAccumulator := 0;
{$ENDIF}
end;

{$IFNDEF SYN_CLX}
procedure TSynEditPrintPreview.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWord = (0, WS_BORDER);
begin
  inherited;
  with Params do begin
    Style := Style or WS_HSCROLL or WS_VSCROLL or BorderStyles[FBorderStyle]
      or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;
{$ENDIF}

function TSynEditPrintPreview.GetPageHeightFromWidth(AWidth: Integer): Integer;
begin
  if Assigned(FSynEditPrint) then begin
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(AWidth, PhysicalHeight, PhysicalWidth);
  end
  else
    Result := MulDiv(AWidth, 141, 100); // fake A4 size
end;

function TSynEditPrintPreview.GetPageWidthFromHeight(AHeight: Integer): Integer;
begin
  if Assigned(FSynEditPrint) then begin
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(AHeight, PhysicalWidth, PhysicalHeight);
  end
  else
    Result := MulDiv(AHeight, 100, 141); // fake A4 size
end;

function TSynEditPrintPreview.GetPageHeight100Percent: Integer;
var
  {$IFNDEF SYN_CLX}
  DC: HDC;
  {$ENDIF}
  ScreenDPI: Integer;
begin
  Result := 0;
{$IFDEF SYN_CLX}
  ScreenDPI := Screen.Height;
{$ELSE}
  DC := GetDC(0);
  ScreenDPI := GetDeviceCaps(DC, LogPixelsY);
  ReleaseDC(0, DC);
{$ENDIF}
  if Assigned(FSynEditPrint) then
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(PhysicalHeight, ScreenDPI, YPixPrInch);
end;

function TSynEditPrintPreview.GetPageWidth100Percent: Integer;
var
  {$IFNDEF SYN_CLX}
  DC: HDC;
  {$ENDIF}
  ScreenDPI: Integer;
begin
  Result := 0;
{$IFDEF SYN_CLX}
  ScreenDPI := Screen.Height;
{$ELSE}
  DC := GetDC(0);
  ScreenDPI := GetDeviceCaps(DC, LogPixelsX);
  ReleaseDC(0, DC);
{$ENDIF}
  if Assigned(FSynEditPrint) then
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(PhysicalWidth, ScreenDPI, XPixPrInch);
end;

procedure TSynEditPrintPreview.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSynEditPrint) then
    SynEditPrint := nil;
end;

procedure TSynEditPrintPreview.PaintPaper;
var
  rcClip, rcPaper: TRect;
  {$IFNDEF SYN_CLX}
  rgnPaper: HRGN;
  {$ENDIF}
  i: Integer;
begin
  with Canvas do begin
      // we work in MM_TEXT mapping mode here...
    rcClip := ClipRect;
    if IsRectEmpty(rcClip) then Exit;
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    if (csDesigning in ComponentState) or (not Assigned(FSynEditPrint)) then begin
      FillRect(rcClip);
      Brush.Color := FPageBG;
      Rectangle(MARGIN_X, MARGIN_Y, MARGIN_X + 30, MARGIN_Y + 43);
      Exit;
    end;
      // fill background around paper
    with rcPaper do begin
      Left := FVirtualOffset.X + FScrollPosition.X;
      if ScaleMode = pscWholePage then
        Top := FVirtualOffset.Y
      else
        Top := FVirtualOffset.Y + FScrollPosition.Y;
      Right := Left + FPageSize.X;
      Bottom := Top + FPageSize.Y;
    {$IFNDEF SYN_CLX}
      rgnPaper := CreateRectRgn(Left, Top, Right + 1, Bottom + 1);
    {$ENDIF}
    end;
  {$IFNDEF SYN_CLX}
    if (NULLREGION <> ExtSelectClipRgn(Handle, rgnPaper, RGN_DIFF)) then
      FillRect(rcClip);
  {$ENDIF}
      // paper shadow
    Brush.Color := clDkGray;
    with rcPaper do begin
      for i := 1 to SHADOW_SIZE do
        PolyLine([Point(Left + i, Bottom + i), Point(Right + i, Bottom + i),
          Point(Right + i, Top + i)]);
    end;
      // paint paper background
  {$IFNDEF SYN_CLX}
    SelectClipRgn(Handle, rgnPaper);
  {$ENDIF}
    Brush.Color := FPageBG;
    with rcPaper do
      Rectangle(Left, Top, Right + 1, Bottom + 1);
  {$IFNDEF SYN_CLX}
    DeleteObject(rgnPaper);
  {$ENDIF}
  end;
end;

procedure TSynEditPrintPreview.Paint;
var
  ptOrgScreen: TPoint;
begin
  with Canvas do begin
    PaintPaper;
    if (csDesigning in ComponentState) or (not Assigned(FSynEditPrint)) then
      Exit;
      // paint the contents, clipped to the area inside of the print margins
      // correct scaling for output:

  {$IFNDEF SYN_CLX}
    SetMapMode(Handle, MM_ANISOTROPIC);
  {$ENDIF}
      // compute the logical point (0, 0) in screen pixels
    with FSynEditPrint.PrinterInfo do
    begin
    {$IFNDEF SYN_CLX}
      SetWindowExtEx(Handle, PhysicalWidth, PhysicalHeight, nil);
      SetViewPortExtEx(Handle, FPageSize.X, FPageSize.Y, nil);
    {$ENDIF}
      ptOrgScreen.X := MulDiv(LeftGutter, FPageSize.X, PhysicalWidth);
      ptOrgScreen.Y := MulDiv(TopGutter, FPageSize.Y, PhysicalHeight);
      Inc(ptOrgScreen.X, FVirtualOffset.X + FScrollPosition.X);
      if ScaleMode = pscWholePage then
        Inc(ptOrgScreen.Y, FVirtualOffset.Y)
      else
        Inc(ptOrgScreen.Y, FVirtualOffset.Y + FScrollPosition.Y);
    {$IFNDEF SYN_CLX}
      SetViewPortOrgEx(Handle, ptOrgScreen.X, ptOrgScreen.Y, nil);
          // clip the output to the print margins
      IntersectClipRect(Handle, 0, 0, PrintableWidth, PrintableHeight);
    {$ENDIF}
    end;
    FSynEditPrint.PrintToCanvas(Canvas, FPageNumber);
  end;
end;

procedure TSynEditPrintPreview.ScrollHorzFor(Value: Integer);
begin
  ScrollHorzTo(FScrollPosition.X + Value);
end;

procedure TSynEditPrintPreview.ScrollHorzTo(Value: Integer);
var
  nW, n: Integer;
begin
  nW := ClientWidth;
  n := nW - FVirtualSize.X;
  if (Value < n) then Value := n;
  if (Value > 0) then Value := 0;
  if (Value <> FScrollPosition.X) then
  begin
    n := Value - FScrollPosition.X;
    FScrollPosition.X := Value;
    UpdateScrollbars;
    if (Abs(n) > nW div 2) then
      Invalidate
    else
    begin
    {$IFNDEF SYN_CLX}
      ScrollWindow(Handle, n, 0, nil, nil);
    {$ENDIF}
      Update;
    end;
  end;
end;

procedure TSynEditPrintPreview.ScrollVertFor(Value: Integer);
begin
  ScrollVertTo(FScrollPosition.Y + Value);
end;

procedure TSynEditPrintPreview.ScrollVertTo(Value: Integer);
var
  nH, n: Integer;
begin
  nH := ClientHeight;
  n := nH - FVirtualSize.Y;
  if (Value < n) then Value := n;
  if (Value > 0) then Value := 0;
  if (Value <> FScrollPosition.Y) then
  begin
    n := Value - FScrollPosition.Y;
    FScrollPosition.Y := Value;
    UpdateScrollbars;
    if (Abs(n) > nH div 2) then
      Invalidate
    else
    begin
    {$IFNDEF SYN_CLX}
      ScrollWindow(Handle, 0, n, nil, nil);
    {$ENDIF}
      Update;
    end;
  end;
end;

procedure TSynEditPrintPreview.SizeChanged;
var
  nWDef: Integer;
begin
  if not (HandleAllocated and Assigned(FSynEditPrint)) then Exit;
  // compute paper size
  case fScaleMode of
    pscWholePage: begin
        FPageSize.X := ClientWidth - 2 * MARGIN_X - SHADOW_SIZE;
        FPageSize.Y := ClientHeight - 2 * MARGIN_Y - SHADOW_SIZE;
        nWDef := GetPageWidthFromHeight(FPageSize.Y);
        if (nWDef < FPageSize.X) then
          FPageSize.X := nWDef
        else
          FPageSize.Y := GetPageHeightFromWidth(FPageSize.X);
      end;
    pscPageWidth: begin
        FPageSize.X := ClientWidth - 2 * MARGIN_X - SHADOW_SIZE;
        FPageSize.Y := GetPageHeightFromWidth(FPageSize.X);
      end;
    pscUserScaled: begin
        FPageSize.X := MulDiv(GetPageWidth100Percent, fScalePercent, 100);
        FPageSize.Y := MulDiv(GetPageHeight100Percent, fScalePercent, 100);
      end;
  end;
  FVirtualSize.X := FPageSize.X + 2 * MARGIN_X + SHADOW_SIZE;
  FVirtualSize.Y := FPageSize.Y + 2 * MARGIN_Y + SHADOW_SIZE;
  FVirtualOffset.X := MARGIN_X;
  if (FVirtualSize.X < ClientWidth) then
    Inc(FVirtualOffset.X, (ClientWidth - FVirtualSize.X) div 2);
  FVirtualOffset.Y := MARGIN_Y;
  if (FVirtualSize.Y < ClientHeight) then
    Inc(FVirtualOffset.Y, (ClientHeight - FVirtualSize.Y) div 2);
  UpdateScrollbars;
// TODO
  FScrollPosition.X := 0;
  FScrollPosition.Y := 0;
end;


procedure TSynEditPrintPreview.UpdateScrollbars;
{$IFNDEF SYN_CLX}
var
  si: TScrollInfo;
{$ENDIF}
begin
{$IFNDEF SYN_CLX}
  FillChar(si, SizeOf(TScrollInfo), 0);
  si.cbSize := SizeOf(TScrollInfo);
  si.fMask := SIF_ALL;
  case FScaleMode of
    pscWholePage: begin
        // hide horizontal scrollbar
        ShowScrollbar(Handle, SB_HORZ, False);
        // show vertical scrollbar, enable if more than one page
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        si.nMin := 1;
        if Assigned(FSynEditPrint) then begin
          si.nMax := FSynEditPrint.PageCount;
          si.nPos := FPageNumber;
        end
        else begin
          si.nMax := 1;
          si.nPos := 1;
        end;
        si.nPage := 1;
        SetScrollInfo(Handle, SB_VERT, si, True);
      end;
    pscPageWidth: begin
        // hide horizontal scrollbar
        ShowScrollbar(Handle, SB_HORZ, False);
        // show vertical scrollbar
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        si.nMax := FVirtualSize.Y;
        si.nPos := -FScrollPosition.Y;
        si.nPage := ClientHeight;
        SetScrollInfo(Handle, SB_VERT, si, True);
      end;
    pscUserScaled: begin
        ShowScrollbar(Handle, SB_HORZ, True);
        ShowScrollbar(Handle, SB_VERT, True);
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        // show horizontal scrollbar
        si.nMax := FVirtualSize.X;
        si.nPos := -FScrollPosition.X;
        si.nPage := ClientWidth;
        SetScrollInfo(Handle, SB_HORZ, si, True);
        // show vertical scrollbar
        si.nMax := FVirtualSize.Y;
        si.nPos := -FScrollPosition.Y;
        si.nPage := ClientHeight;
        SetScrollInfo(Handle, SB_VERT, si, True);
      end;
  end;
{$ENDIF}
end;

procedure TSynEditPrintPreview.SetBorderStyle(Value: TBorderStyle);
begin
  if (Value <> FBorderStyle) then
  begin
    FBorderStyle := Value;
  {$IFNDEF SYN_CLX}
    RecreateWnd;
  {$ENDIF}
  end;
end;

procedure TSynEditPrintPreview.SetPageBG(Value: TColor);
begin
  if (FPageBG <> Value) then
  begin
    FPageBG := Value;
    Invalidate;
  end;
end;

procedure TSynEditPrintPreview.SetSynEditPrint(Value: TSynEditPrint);
begin
  if (FSynEditPrint <> Value) then
  begin
    FSynEditPrint := Value;
    if Assigned(FSynEditPrint) then
      FSynEditPrint.FreeNotification(Self);
  end;
end;

procedure TSynEditPrintPreview.SetScaleMode(Value: TSynPreviewScale);
begin
  if (FScaleMode <> Value) then begin
    FScaleMode := Value;
    FScrollPosition := Point(0, 0);
    SizeChanged;
    if Assigned(FOnScaleChange) then                                            // JD 2002-01-9
      FOnScaleChange(Self);                                                     // JD 2002-01-9
    Invalidate;
  end;
end;

procedure TSynEditPrintPreview.SetScalePercent(Value: Integer);
begin
  if (FScalePercent <> Value) then begin
    FScaleMode := pscUserScaled;
    FScrollPosition := Point(0, 0);
    FScalePercent := Value;
    SizeChanged;
    Invalidate;
  end else
    ScaleMode := pscUserScaled;
  if Assigned(FOnScaleChange) then                                              // JD 2002-01-9
    FOnScaleChange(Self);                                                       // JD 2002-01-9
end;

{$IFNDEF SYN_CLX}
procedure TSynEditPrintPreview.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TSynEditPrintPreview.WMHScroll(var Msg: TWMHScroll);
var
  nW: Integer;
begin
  if (FScaleMode <> pscWholePage) then begin
    nW := ClientWidth;
    case Msg.ScrollCode of
      SB_TOP: ScrollHorzTo(0);
      SB_BOTTOM: ScrollHorzTo(-FVirtualSize.X);
      SB_LINEDOWN: ScrollHorzFor(-(nW div 10));
      SB_LINEUP: ScrollHorzFor(nW div 10);
      SB_PAGEDOWN: ScrollHorzFor(-(nW div 2));
      SB_PAGEUP: ScrollHorzFor(nW div 2);
      SB_THUMBPOSITION, SB_THUMBTRACK: ScrollHorzTo(-Msg.Pos);
    end;
  end;
end;

procedure TSynEditPrintPreview.WMSize(var Msg: TWMSize);
begin
  inherited;
  if not (csDesigning in ComponentState) then SizeChanged;
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Visible := FALSE;
  end;
  Result := ScrollHintWnd;
end;

procedure TSynEditPrintPreview.WMVScroll(var Msg: TWMVScroll);
var
  nH: Integer;
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
begin
  if (FScaleMode = pscWholePage) then begin
    if Assigned(FSynEditPrint) then
      case Msg.ScrollCode of
        SB_TOP: FPageNumber := 1;
        SB_BOTTOM: FPageNumber := FSynEditPrint.PageCount;
        SB_LINEDOWN, SB_PAGEDOWN: begin
            FPageNumber := FPageNumber + 1;
            if FPageNumber > FSynEditPrint.PageCount then
              FPageNumber := FSynEditPrint.PageCount;
          end;
        SB_LINEUP, SB_PAGEUP: begin
            FPageNumber := FPageNumber - 1;
            if FPageNumber < 1 then
              FPageNumber := 1;
          end;
        SB_THUMBPOSITION, SB_THUMBTRACK: begin
            FPageNumber := Msg.Pos;
              //Showing hint window - principle copied from SynEdit.pas
            if FShowScrollHint then begin
              ScrollHint := GetScrollHint;
              if not ScrollHint.Visible then begin
                ScrollHint.Color := Application.HintColor;
                ScrollHint.Visible := TRUE;
              end;
              s := Format(SYNS_PreviewScrollInfoFmt, [FPageNumber]);
{$IFDEF SYN_COMPILER_3_UP}
              rc := ScrollHint.CalcHintRect(200, s, nil);
{$ELSE}
              rc := Rect(0, 0, ScrollHint.Canvas.TextWidth(s) + 6,
                ScrollHint.Canvas.TextHeight(s) + 4);
{$ENDIF}
              pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));
              OffsetRect(rc, pt.x, pt.y);
              ScrollHint.ActivateHint(rc, s);
{$IFDEF SYN_COMPILER_3}
              SendMessage(ScrollHint.Handle, WM_NCPAINT, 1, 0);
{$ENDIF}
{$IFNDEF SYN_COMPILER_3_UP}
              ScrollHint.Invalidate;
{$ENDIF}
              ScrollHint.Update;
            end;
          end;
        SB_ENDSCROLL: begin
            if FShowScrollHint then
            begin
              ScrollHint := GetScrollHint;
              ScrollHint.Visible := False;
              ShowWindow(ScrollHint.Handle, SW_HIDE);
            end;
          end;
      end;
      {Updating scroll position and redrawing}
    FScrollPosition.Y := -(FPageNumber - 1);
    UpdateScrollbars;
    if Assigned(FOnPreviewPage) then
      FOnPreviewPage(Self, FPageNumber);
    Invalidate;
  end
  else begin
    nH := ClientHeight;
    case Msg.ScrollCode of
      SB_TOP: ScrollVertTo(0);
      SB_BOTTOM: ScrollVertTo(-FVirtualSize.Y);
      SB_LINEDOWN: ScrollVertFor(-(nH div 10));
      SB_LINEUP: ScrollVertFor(nH div 10);
      SB_PAGEDOWN: ScrollVertFor(-(nH div 2));
      SB_PAGEUP: ScrollVertFor(nH div 2);
      SB_THUMBPOSITION, SB_THUMBTRACK: ScrollVertTo(-Msg.Pos);
    end;
  end;
end;

procedure TSynEditPrintPreview.WMMouseWheel(var Message: TWMMouseWheel);
{$IFNDEF SYN_COMPILER_3_UP}
const
  WHEEL_DELTA = 120;
{$ENDIF}

  procedure MouseWheelUp;
  begin
    ScrollVertFor(WHEEL_DELTA);
  end;

  procedure MouseWheelDown;
  begin
    ScrollVertFor(-WHEEL_DELTA);
  end;

var
  MousePos: TPoint;
  IsNeg: Boolean;
begin
  MousePos:= SmallPointToPoint(Message.Pos);
  Inc(FWheelAccumulator, Message.WheelDelta);
  while Abs(FWheelAccumulator) >= WHEEL_DELTA do
  begin
    IsNeg := FWheelAccumulator < 0;
    FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
    if IsNeg then
    begin
      if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
      MouseWheelDown;
    end
    else
      MouseWheelUp;
  end;
end;

{$ENDIF}

procedure TSynEditPrintPreview.UpdatePreview;
var
  OldScale: Integer;
  OldMode: TSynPreviewScale;
begin
  OldScale := ScalePercent;
  OldMode := ScaleMode;
  ScalePercent := 100;
  if Assigned(FSynEditPrint) then
    FSynEditPrint.UpdatePages(Canvas);
  SizeChanged;
  Invalidate;
  ScaleMode := OldMode;
  if ScaleMode = pscUserScaled then
    ScalePercent := OldScale;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
end;

procedure TSynEditPrintPreview.FirstPage;
begin
  FPageNumber := 1;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.LastPage;
begin
  if Assigned(FSynEditPrint) then
    FPageNumber := FSynEditPrint.PageCount;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.NextPage;
begin
  FPageNumber := FPageNumber + 1;
  if Assigned(FSynEditPrint) and (FPageNumber > FSynEditPrint.PageCount) then
    FPageNumber := FSynEditPrint.PageCount;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.PreviousPage;
begin
  FPageNumber := FPageNumber - 1;
  if Assigned(FSynEditPrint) and (FPageNumber < 1) then
    FPageNumber := 1;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.Print;
begin
  if Assigned(FSynEditPrint) then begin
    FSynEditPrint.Print;
    UpdatePreview;
  end;
end;

function TSynEditPrintPreview.GetPageCount: Integer;
begin
  Result := SynEditPrint.PageCount;
end;

end.
