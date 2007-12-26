unit Handles;

{ TStretchHandles is a transparent control to implement runtime grab handles
  for Forms Designer-like projects.  It paints the handles on its own canvas,
  maintains a list of the controls it is supposed to manage, and traps mouse
  and keyboard events to move/resize itself and its child controls.  See the
  accompanying README file for more information.

  Distributed by the author as freeware, please do not sell.

  Anthony Scott
  CIS: 75567,3547                                                              }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Menus, StdCtrls, Dialogs;
                                       { miscellaneous type declarations }
type
  TDragStyle = (dsMove, dsSizeTopLeft, dsSizeTopRight, dsSizeBottomLeft, dsSizeBottomRight,
                dsSizeTop, dsSizeLeft, dsSizeBottom, dsSizeRight);
  TForwardMessage = (fmMouseDown, fmMouseUp);
  GridValues = 1..32;
  EBadChild = class(Exception);
                                       { TStretchHandle component declaration }
type
  TStretchHandle = class(TCustomControl)
  private
    FDragOffset: TPoint;
    FDragStyle: TDragStyle;
    FDragging: boolean;
    FDragRect: TRect;
    FLocked: boolean;
    FPrimaryColor: TColor;
    FSecondaryColor: TColor;
    FGridX, FGridY: GridValues;
    FChildList: TList;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDLGCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure Rubberband(XPos, YPos: integer; ShowBox: boolean);
    procedure ForwardMessage(FwdMsg: TForwardMessage; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetPrimaryColor(Color: TColor);
    procedure SetSecondaryColor(Color: TColor);
    procedure SetGridState(Value: boolean);
    function GetGridState: boolean;
    function GetChildCount: integer;
    function GetChildControl(idx: integer): TControl;
    function GetModifiedRect(XPos, YPos: integer): TRect;
    function PointOverChild(P: TPoint): boolean;
    function XGridAdjust(X: integer): integer;
    function YGridAdjust(Y: integer): integer;
    function IsAttached: boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var key: Word; Shift: TShiftState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    property Canvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach(ChildControl: TControl);
    procedure Detach;
    procedure ReleaseChild(ChildControl: TControl);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BringToFront;
    procedure SendToBack;
    procedure SetColors(Color1, Color2: TColor);
    function IndexOf(ChildControl: TControl): integer;
                                       { new run-time only properties }
    property Attached: boolean read IsAttached;
    property ChildCount: integer read GetChildCount;
    property Children[idx: integer]: TControl read GetChildControl;
  published
                                       { new properties }
    property Color: TColor read FPrimaryColor write SetPrimaryColor default clBlack;
    property SecondaryColor: TColor read FSecondaryColor write SetSecondaryColor default clGray;
    property Locked: boolean read FLocked write FLocked default False;
    property GridX: GridValues read FGridX write FGridX default 8;
    property GridY: GridValues read FGridY write FGridY default 8;
    property SnapToGrid: boolean read GetGridState write SetGridState default False;
                                       { inherited properties }
    property DragCursor;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
                                       { defined events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

procedure Register;
function MinInt(a, b: integer): integer;
function MaxInt(a, b: integer): integer;

implementation

procedure Register;
begin
                                       { add the component to the 'Samples' tab }
  RegisterComponents('Samples', [TStretchHandle]);

end;

constructor TStretchHandle.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
                                       { create storage for child objects }
  FChildList := TList.Create;
                                       { initialize default properties }
  Width := 24;
  Height := 24;
  FPrimaryColor := clBlack;
  FSecondaryColor := clGray;
                                       { a value of 1 is used to effectively disable the snap-to grid }
  FGridX := 1;
  FGridY := 1;
                                       { doesn't do anything until it is Attached to something else }
  Enabled := False;
  Visible := False;

end;

destructor TStretchHandle.Destroy;
begin
                                       { tidy up carefully }                                                                
  FChildList.Free;
  inherited Destroy;

end;

procedure TStretchHandle.CreateParams(var Params: TCreateParams);
begin
                                       { set default Params values }
  inherited CreateParams(Params);
                                       { then add transparency; ensures correct repaint order }
  Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;

end;

procedure TStretchHandle.WMGetDLGCode(var Message: TMessage);
begin
                                       { get arrow key press events }
  Message.Result := DLGC_WANTARROWS;

end;

procedure TStretchHandle.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
                                       { completely fake erase, don't call inherited, don't collect $200 }
  Message.Result := 1;

end;

procedure TStretchHandle.Attach(ChildControl: TControl);
var
  L, T, W, H: integer;
begin
                                       { definitely not allowed! }
  if ChildControl is TForm then
    raise EBadChild.Create('Handles can not be attached to a Form!');
                                       { add child component to unique list managed by TStretchHandle }
  if (ChildControl <> nil) and (FChildList.IndexOf(TObject(ChildControl)) = -1) then
    begin
                                       { make sure new child's Parent matches siblings }
      if (FChildList.Count > 0) and (ChildControl.Parent <> Parent) then
        Detach;
                                       { initialize when first child is attached }
      if FChildList.Count = 0 then
        begin
          Parent := ChildControl.Parent;
                                       { only make it visible now, to avoid color flashing, & accept events }
          FDragRect := Rect(0, 0, 0, 0);
          Enabled := True;
          Visible := True;
          inherited SetBounds(ChildControl.Left - 2, ChildControl.Top - 2, ChildControl.Width + 5, ChildControl.Height + 5);

        end
      else
        begin
                                       { set size to bound all children, plus room for handles }
          L := MinInt(Left, ChildControl.Left - 2);
          T := MinInt(Top, ChildControl.Top - 2);
          W := Maxint(Left + Width - 3, ChildControl.Left + ChildControl.Width) - L + 3;
          H := Maxint(Top + Height - 3, ChildControl.Top + ChildControl.Height) - T + 3;
          inherited SetBounds(L, T, W, H);

        end;
                                       { add to list of active Children }
      FChildList.Add(TObject(ChildControl));
                                       { re-set DragStyle }
      FDragStyle := dsMove;
                                       { use old BringToFront so as not to change Child's Z-order }
      if not (csDesigning in ComponentState) then
        begin
          inherited BringToFront;
                                       { allow us to get Mouse events immediately! }
          SetCapture(Handle);
                                       { get keyboard events }
          if Visible and Enabled then
            SetFocus;
        end;

    end;

end;

procedure TStretchHandle.Detach;
begin
                                       { remove all Child components from list }
  if FChildList.Count > 0 then
    with FChildList do
      repeat
        Delete(0);
      until Count = 0;
                                       { disable & hide StretchHandle }
  FLocked := False;
  Width := 24;
  Height := 24;
  Enabled := False;
  Visible := False;
  Parent := nil;
  FDragRect := Rect(0, 0, 0, 0);

end;

procedure TStretchHandle.ReleaseChild(ChildControl: TControl);
var
  idx, L, T, W, H: integer;
  AControl: TControl;
begin
                                       { delete the Child if it exists in the list }
  idx := FChildList.IndexOf(TObject(ChildControl));
  if (ChildControl <> nil) and (idx >= 0) then
    FChildList.Delete(idx);
                                       { disable & hide StretchHandle if no more children }
  if FChildList.Count = 0 then
    begin
      FLocked := False;
      Enabled := False;
      Visible := False;
      Parent := nil;
      FDragRect := Rect(0, 0, 0, 0);
    end
  else
    begin
                                       { set size to bound remaining children, plus room for handles }
      L := TControl(FChildList.Items[0]).Left - 2;
      T := TControl(FChildList.Items[0]).Top - 2;
      W := TControl(FChildList.Items[0]).Width + 3;
      H := TControl(FChildList.Items[0]).Height + 3;

      for idx := 0 to FChildList.Count - 1 do
        begin
          AControl := TControl(FChildList.Items[idx]);
          L := MinInt(L, AControl.Left - 2);
          T := MinInt(T, AControl.Top - 2);
          W := Maxint(L + W - 3, AControl.Left + AControl.Width) - L + 3;
          H := Maxint(T + H - 3, AControl.Top + AControl.Height) - T + 3;
        end;

      inherited SetBounds(L, T, W, H);

    end;

end;

function TStretchHandle.IndexOf(ChildControl: TControl): integer;
begin
                                       { simply pass on the result... }
  Result := FChildList.IndexOf(TObject(ChildControl));

end;

procedure TStretchHandle.BringToFront;
var
  i: integer;
begin
                                       { do nothing if not Attached }
  if Attached and not Locked then
    begin
                                       { take care of Children first, in Attach order }
      for i := 0 to FChildList.Count - 1 do
        begin
          TControl(FChildList[i]).BringToFront;
        end;
                                       { make sure keyboard focus is restored }
      inherited BringToFront;
      if Visible and Enabled then
        SetFocus;
    end;

end;

procedure TStretchHandle.SendToBack;
var
  i: integer;
begin
                                       { do nothing if not Attached }
  if Attached and not Locked then
    begin
                                       { take care of Children first, in Attach order }
      for i := 0 to FChildList.Count - 1 do
        begin
          TControl(FChildList[i]).SendToBack;
        end;
                                       { Handles stay in front of everything, always }
      inherited BringToFront;
      if Visible and Enabled then
        SetFocus;
    end;

end;

procedure TStretchHandle.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
                                       { only process MouseDown if it is over a Child, else forward }
  if PointOverChild(Point(Left + X, Top + Y)) then
    begin
      if (Button = mbLeft) and not FLocked then
        begin
          FDragOffset := Point(X, Y);
          FDragging := True;
        end;
      inherited MouseDown(Button, Shift, X, Y);
    end
  else
    begin
      Cursor := crDefault;
      SetCursor(Screen.Cursors[Cursor]);
      ForwardMessage(fmMouseDown, Button, Shift, Left + X, Top + Y);
    end;

end;

procedure TStretchHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARect: TRect;
begin
                                       { resize, reposition if anything changed }
  if FDragging and (Button = mbLeft) then
    begin
                                       { disallow drop off Parent }
      if (Left + X) < 0 then
        X := -Left;
      if (Top + Y) < 0 then
        Y := -Top;
      if (Left + X) > Parent.Width then
        X := Parent.Width - Left;
      if (Top + Y) > Parent.Height then
        Y := Parent.Height - Top;
                                       { force Paint when size doesn't change but position does }
      if (X <> FDragOffset.X) or (Y <> FDragOffset.Y) then
        begin
          Invalidate;
          ARect := GetModifiedRect(X, Y);
          SetBounds(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
        end;
                                       { clear drag outline }
      RubberBand(0, 0, False);
                                       { seem to need this for keyboard events }
      if Visible and Enabled then
        SetFocus;

      FDragging := False;
      Cursor := crDefault;
      ReleaseCapture;
                                       { perform default processing }
      inherited MouseUp(Button, Shift, X, Y);

    end
  else
    ForwardMessage(fmMouseUp, Button, Shift, Left + X, Top + Y);

end;

procedure TStretchHandle.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ARect: TRect;
  DragStyle: TDragStyle;
begin
                                       { this may be a move immediately on Attach instead of MouseDown }
  if (ssLeft in Shift) and not FDragging and not FLocked then
    begin
      FDragOffset := Point(X, Y);
      FDragging := True;
    end
                                       { only recognize move after simulated MouseDown }
  else
    begin
                                       { let's not hog mouse events unnecessarily } 
      if not (ssLeft in Shift) then
        ReleaseCapture;
                                       { default to drag cursor only when dragging }
      DragStyle := dsMove;
      Cursor := crDefault;
                                       { disallow resize if multiple children }
      if FChildList.Count = 1 then
        begin

          ARect := GetClientRect;
                                       { so I don't like long nested if statements... }
          if ((Abs(X - ARect.Left) < 5) and (Abs(Y - ARect.Top) < 5)) then
            begin
              DragStyle := dsSizeTopLeft;
              Cursor := crSizeNWSE;
            end;

          if ((Abs(X - ARect.Right) < 5) and (Abs(Y - ARect.Bottom) < 5)) then
            begin
              DragStyle := dsSizeBottomRight;
              Cursor := crSizeNWSE;
            end;

          if ((Abs(X - ARect.Right) < 5) and (Abs(Y - ARect.Top) < 5)) then
            begin
              DragStyle := dsSizeTopRight;
              Cursor := crSizeNESW;
            end;

          if ((Abs(X - ARect.Left) < 5) and (Abs(Y - ARect.Bottom) < 5)) then
            begin
              DragStyle := dsSizeBottomLeft;
              Cursor := crSizeNESW;
            end;

          if ((Abs(X - trunc(ARect.Right - ARect.Left) / 2) < 3) and (Abs(Y - ARect.Top) < 5)) then
            begin
              DragStyle := dsSizeTop;
              Cursor := crSizeNS;
            end;

          if ((Abs(X - trunc(ARect.Right - ARect.Left) / 2) < 3) and (Abs(Y - ARect.Bottom) < 5)) then
            begin
              DragStyle := dsSizeBottom;
              Cursor := crSizeNS;
            end;

          if ((Abs(Y - trunc(ARect.Bottom - ARect.Top) / 2) < 3) and (Abs(X - ARect.Left) < 5)) then
            begin
              DragStyle := dsSizeLeft;
              Cursor := crSizeWE;
            end;

          if ((Abs(Y - trunc(ARect.Bottom - ARect.Top) / 2) < 3) and (Abs(X - ARect.Right) < 5)) then
            begin
              DragStyle := dsSizeRight;
              Cursor := crSizeWE;
            end;

        end;
                                       { if position-locked, override cursor change }
      if FLocked then
        Cursor := crNoDrop;

      if FDragging then
        begin
                                       { disallow drag off Parent }
          if (Left + X) < 0 then
            X := -Left;
          if (Top + Y) < 0 then
            Y := -Top;
          if (Left + X) > Parent.Width then
            X := Parent.Width - Left;
          if (Top + Y) > Parent.Height then
            Y := Parent.Height - Top;
                                       { display cursor & drag outline }
          if FDragStyle = dsMove then
            Cursor := DragCursor;
          SetCursor(Screen.Cursors[Cursor]);
          RubberBand(X, Y, True);

        end
      else
        FDragStyle := DragStyle;

  end;
                                       { perform default processing }
  inherited MouseMove(Shift, X, Y);

end;

procedure TStretchHandle.ForwardMessage(FwdMsg: TForwardMessage; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  Found: boolean;
  Msg: Word;
  ARect: TRect;
  AControl: TControl;
  AMessage: TMessage;
begin
                                       { construct the message to be sent }
  case FwdMsg of
    fmMouseDown:
      case Button of
        mbLeft:
          Msg := WM_LBUTTONDOWN;
        mbMiddle:
          Msg := WM_MBUTTONDOWN;
        mbRight:
          Msg := WM_RBUTTONDOWN;
      end;
    fmMouseUp:
      case Button of
        mbLeft:
          Msg := WM_LBUTTONUP;
        mbMiddle:
          Msg := WM_MBUTTONUP;
        mbRight:
          Msg := WM_RBUTTONUP;
      end;
  end;

  AMessage.WParam := 0;
                                       { determine whether X, Y is over any other windowed control }
  Found := False;
  for i := 0 to Parent.ControlCount - 1 do
    begin
      AControl := TControl(Parent.Controls[i]);
      if (AControl is TWinControl) and not (AControl is TStretchHandle) then
        begin
          ARect := Rect(AControl.Left,
                        AControl.Top,
                        AControl.Left + AControl.Width,
                        AControl.Top + AControl.Height);
                                        { X, Y are relative to Parent }
          if PtInRect(ARect, Point(X, Y)) then
            begin
              Found := True;
              break;
            end;
        end;
    end;
                                        { forward the message to the control if found, else to the Parent }
  if Found then
    begin
      AMessage.LParamLo := X - AControl.Left;
      AMessage.LParamHi := Y - AControl.Top;
      SendMessage(TWinControl(AControl).Handle, Msg, AMessage.WParam, AMessage.LParam);
    end
  else
    begin
      AMessage.LParamLo := X;
      AMessage.LParamHi := Y;
      SendMessage(Parent.Handle, Msg, AMessage.WParam, AMessage.LParam);
    end;

end;

procedure TStretchHandle.KeyDown(var Key: Word; Shift: TShiftState);
begin
                                       { process arrow keys to move/resize Handles & Child, also move siblings }
  case Key of
    VK_UP:
      begin
        Invalidate;
        SetBounds(Left, Top - 1, Width, Height);
      end;
    VK_DOWN:
      begin
        Invalidate;
        SetBounds(Left, Top + 1, Width, Height);
      end;
    VK_LEFT:
      begin
        Invalidate;
        SetBounds(Left - 1, Top, Width, Height);
      end;
    VK_RIGHT:
      begin
        Invalidate;
        SetBounds(Left + 1, Top, Width, Height);
      end;
  end;

  inherited KeyDown(Key, Shift);

end;

function TStretchHandle.GetModifiedRect(XPos, YPos: integer): TRect;
var
  ARect: TRect;
begin
                                       { compute new position/size, depending on FDragStyle}
  case FDragStyle of

    dsSizeTopLeft:
      begin
        ARect.Left := XGridAdjust(Left + (XPos - FDragOffset.X)) - 2;
        ARect.Top := YGridAdjust(Top + (YPos - FDragOffset.Y)) - 2;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height - (ARect.Top - Top);
      end;

    dsSizeTopRight:
      begin
        ARect.Left := Left;
        ARect.Top := YGridAdjust(Top + (YPos - FDragOffset.Y)) - 2;
        ARect.Right := XGridAdjust(Width + (XPos - FDragOffset.X)) - 3;
        ARect.Bottom := Height - (ARect.Top - Top);
      end;

    dsSizeBottomLeft:
      begin
        ARect.Left := XGridAdjust(Left + (XPos - FDragOffset.X)) - 2;
        ARect.Top := Top;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := YGridAdjust(Height + (YPos - FDragOffset.Y)) - 3;
      end;

    dsSizeBottomRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := XGridAdjust(Width + (XPos - FDragOffset.X)) - 3;
        ARect.Bottom := YGridAdjust(Height + (YPos - FDragOffset.Y)) - 3;
      end;

    dsSizeTop:
      begin
        ARect.Left := Left;
        ARect.Top := YGridAdjust(Top + (YPos - FDragOffset.Y)) - 2;
        ARect.Right := Width;
        ARect.Bottom := Height - (ARect.Top - Top);
      end;

    dsSizeBottom:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width;
        ARect.Bottom := YGridAdjust(Height + (YPos - FDragOffset.Y)) - 3;
      end;

    dsSizeLeft:
      begin
        ARect.Left := XGridAdjust(Left + (XPos - FDragOffset.X)) - 2;
        ARect.Top := Top;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height;
      end;

    dsSizeRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := XGridAdjust(Width + (XPos - FDragOffset.X)) - 3;
        ARect.Bottom := Height;
      end;

  else
                                       { keep size, move to new position }
    ARect.Left := XGridAdjust(Left + (XPos - FDragOffset.X)) - 2;
    ARect.Top := YGridAdjust(Top + (YPos - FDragOffset.Y)) - 2;
    ARect.Right := Width;
    ARect.Bottom := Height;

  end;
                                       { impose a minimum size for sanity }
  if ARect.Right < 5 then
    ARect.Right := 5;
  if ARect.Bottom < 5 then
    ARect.Bottom := 5;

  Result := ARect;

end;

procedure TStretchHandle.Rubberband(XPos, YPos: integer; ShowBox: boolean);
var
  NewRect: TRect;
  PtA, PtB: TPoint;
  ScreenDC: HDC;
begin
                                       { outline is drawn over all windows }
  ScreenDC := GetDC(0);
                                       { erase previous rectangle, if any, & adjust for handle's position }
  if (FDragRect.Left <> 0) or (FDragRect.Top <> 0) or (FDragRect.Right <> 0) or (FDragRect.Bottom <> 0) then
    begin
      PtA := Parent.ClientToScreen(Point(FDragRect.Left + 2, FDragRect.Top + 2));
      PtB := Parent.ClientToScreen(Point(FDragRect.Left + FDragRect.Right - 3, FDragRect.Top + FDragRect.Bottom - 3));
      DrawFocusRect(ScreenDC, Rect(PtA.X, PtA.Y, PtB.X, PtB.Y));
      FDragRect := Rect(0, 0, 0, 0);
    end;
                                       { draw new rectangle unless this is a final erase }
  if ShowBox then
    begin
      NewRect := GetModifiedRect(XPos, YPos);
      PtA := Parent.ClientToScreen(Point(NewRect.Left + 2, NewRect.Top + 2));
      PtB := Parent.ClientToScreen(Point(NewRect.Left + NewRect.Right - 3, NewRect.Top + NewRect.Bottom - 3));
      DrawFocusRect(ScreenDC, Rect(PtA.X, PtA.Y, PtB.X, PtB.Y));
      FDragRect := NewRect;
    end
  else
    begin
      Parent.Repaint;
      Repaint;
    end;

  ReleaseDC(0, ScreenDC);

end;

procedure TStretchHandle.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  WasVisible: boolean;
  i: integer;
  AControl: TControl;
begin
                                       { hide & preserve fixed size in design mode }
  WasVisible := Visible;
  if csDesigning in ComponentState then
    begin
      Visible := False;
      inherited SetBounds(ALeft, ATop, 24, 24);
    end
  else                                 { move child also, if any (but only if not locked) }
    if not FLocked then
      begin
        for i := 0 to FChildList.Count - 1 do
          begin
            AControl := FChildList[i];
            AControl.SetBounds(AControl.Left - Left + ALeft,
                               AControl.Top - Top + ATop,
                               AControl.Width - Width + AWidth,
                               AControl.Height - Height + AHeight);
          end;
        inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      end;
                                       { restore visibility }
  if Visible = False then
    Visible := WasVisible;

end;

procedure TStretchHandle.Paint;
var
   AControl: TControl;
   ARect, BoxRect: TRect;
   i: integer;
begin

  inherited Paint;
                                        { do it differently at design time... }
  if csDesigning in ComponentState then
    begin
      Canvas.Brush.Color := FPrimaryColor;
      BoxRect := Rect(0, 0, 5, 5);
      Canvas.FillRect(BoxRect);
      BoxRect := Rect(19, 0, 24, 5);
      Canvas.FillRect(BoxRect);
      BoxRect := Rect(19, 19, 24, 24);
      Canvas.FillRect(BoxRect);
      BoxRect := Rect(0, 19, 5, 24);
      Canvas.FillRect(BoxRect);
    end
  else
    begin
                                       { set color to primary if only one child, else secondary }
      if FChildList.Count = 1 then
        Canvas.Brush.Color := FPrimaryColor
      else
        Canvas.Brush.Color := FSecondaryColor;
                                       { draw resize handles for each child }
      for i := 0 to FChildList.Count - 1 do
        begin

          AControl := TControl(FChildList.Items[i]);
          ARect := Rect(AControl.Left - Left - 2,
                        AControl.Top - Top - 2,
                        AControl.Left - Left + AControl.Width + 2,
                        AControl.Top - Top + AControl.Height + 2);

          with Canvas do
            begin
                                       { draw corner boxes (assuming Canvas is minimum 5x5) }
              BoxRect := Rect(ARect.Left, ARect.Top, ARect.Left + 5, ARect.Top + 5);
              FillRect(BoxRect);
              BoxRect := Rect(ARect.Right - 5, ARect.Top, ARect.Right, ARect.Top + 5);
              FillRect(BoxRect);
              BoxRect := Rect(ARect.Right - 5, ARect.Bottom - 5, ARect.Right, ARect.Bottom);
              FillRect(BoxRect);
              BoxRect := Rect(ARect.Left, ARect.Bottom - 5, ARect.Left + 5, ARect.Bottom);
              FillRect(BoxRect);
                                       { only for single Children, draw center boxes }
              if FChildList.Count = 1 then
                begin
                  BoxRect := Rect(ARect.Left + trunc((ARect.Right - ARect.Left) / 2) - 2,
                                  ARect.Top,
                                  ARect.Left + trunc((ARect.Right - ARect.Left) / 2) + 3,
                                  ARect.Top + 5);
                  FillRect(BoxRect);
                  BoxRect := Rect(ARect.Left + trunc((ARect.Right - ARect.Left) / 2) - 2,
                                  ARect.Bottom - 5,
                                  ARect.Left + trunc((ARect.Right - ARect.Left) / 2) + 3,
                                  ARect.Bottom);
                  FillRect(BoxRect);
                  BoxRect := Rect(ARect.Left,
                                  ARect.Top + trunc((ARect.Bottom - ARect.Top) / 2) - 2,
                                  ARect.Left + 5,
                                  ARect.Top + trunc((ARect.Bottom - ARect.Top) / 2) + 3);
                  FillRect(BoxRect);
                  BoxRect := Rect(ARect.Right - 5,
                                  ARect.Top + trunc((ARect.Bottom - ARect.Top) / 2) - 2,
                                  ARect.Right,
                                  ARect.Top + trunc((ARect.Bottom - ARect.Top) / 2) + 3);
                  FillRect(BoxRect);
                end;

            end;

        end;

    end;

end;

procedure TStretchHandle.SetPrimaryColor(Color: TColor);
begin
                                       { set single select color, repaint immediately }
  FPrimaryColor := Color;
  Repaint;

end;

procedure TStretchHandle.SetSecondaryColor(Color: TColor);
begin
                                       { set multiple select color, repaint immediately }
  FSecondaryColor := Color;
  Repaint;

end;

procedure TStretchHandle.SetColors(Color1, Color2: TColor);
begin
                                       { set single/multiple select colors, repaint }
  FPrimaryColor := Color1;
  FSecondaryColor := Color2;
  Repaint;

end;

procedure TStretchHandle.SetGridState(Value: boolean);
begin
                                       { a value of 1 effectively disables a grid axis }
  if Value then
    begin
      FGridX := 8;
      FGridY := 8;
    end
  else
    begin
      FGridX := 1;
      FGridY := 1;
    end;

end;

function TStretchHandle.GetGridState: boolean;
begin

  if (FGridX > 1) or (FGridY > 1) then
    Result := True
  else
    Result := False;

end;

function TStretchHandle.GetChildCount: integer;
begin
  Result := FChildList.Count;
end;

function TStretchHandle.GetChildControl(idx: integer): TControl;
begin

  if (FChildList.Count > 0) and (idx >= 0) then
    Result := FChildList[idx]
  else
    Result := nil;

end;

function TStretchHandle.IsAttached: boolean;
begin

  if FChildList.Count > 0 then
    Result := True
  else
    Result := False;

end;

function TStretchHandle.PointOverChild(P: TPoint): boolean;
var
  i: integer;
  ARect: TRect;
  AControl: TControl;
begin
                                       { determine whether X, Y is over any child (for dragging) }
  Result := False;
  for i := 0 to FChildList.Count - 1 do
    begin
      AControl := TControl(FChildList[i]);
      ARect := Rect(AControl.Left - 2,
                    AControl.Top - 2,
                    AControl.Left + AControl.Width + 2,
                    AControl.Top + AControl.Height + 2);
                                       { P is relative to the Parent }
      if PtInRect(ARect, P) then
        begin
          Result := True;
          break;
        end;
    end;

end;

function TStretchHandle.XGridAdjust(X: integer): integer;
begin
  Result := (X DIV FGridX) * FGridX;
end;

function TStretchHandle.YGridAdjust(Y: integer): integer;
begin
  Result := (Y DIV FGridY) * FGridY;
end;

function MinInt(a, b: integer): integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function MaxInt(a, b: integer): integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

end.
