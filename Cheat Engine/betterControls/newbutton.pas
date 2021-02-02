unit newButton;

{$mode objfpc}{$H+}

interface

uses
  windows, UxTheme, Classes, SysUtils, StdCtrls, LCLType, Graphics, LMessages, Controls;

type
  TNewButton=class(StdCtrls.TButton)
  private
    painted: boolean;
    MouseIsDown: boolean;
    fCanvas: TCanvas;
    fCustomDraw: boolean;
    fOnPaint: TNotifyEvent;

    fFaceColorDisabled: TColor;
    fFaceColorUp: TColor;
    fFaceColorDown: TColor;
    fFaceColorHover: TColor;
    fBorderColor: TColor;
    fInactiveBorderColor: TColor;
    fInactiveFontColor: TColor;
    fPenColorHover: TColor;

    fDarkMode: boolean;
    function getColor: TColor;
  protected
    procedure SetColor(c: TColor); override;
    procedure DefaultCustomPaint;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ChildHandlesCreated; override;
  published
  {  property CustomDraw: boolean read fCustomDraw write fCustomDraw;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
    property Canvas: TCanvas read fCanvas;
    property Color: TColor read getColor write setColor;
    property FaceColorUp: TColor read fFaceColorUp write fFaceColorDown;
    property FaceColorDown: TColor read fFaceColorDown write fFaceColorDown;
    property FaceColorHover: TColor read fFaceColorHover write fFaceColorHover;
    property FaceColorDisabled: TColor read fFaceColorDisabled write fFaceColorDisabled;
    property BorderColor: TColor read fBorderColor write fBorderColor;
    property InactiveBorderColor: TColor read fInactiveBorderColor write fInactiveBorderColor;
    property InactiveFontColor: TColor read fInactiveFontColor write fInactiveFontColor;   }
  end;



implementation

uses betterControls;

procedure TNewButton.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode and (Parent<>nil) then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(handle, 'EXPLORER', nil);
  end;
end;

function TNewButton.getColor: Tcolor;
begin
  result:=inherited color;
end;

procedure TNewButton.setColor(c: TColor);
var
  newc: longint;
  r,g,b: byte;
begin
  inherited setColor(c);

  if ShouldAppsUseDarkMode then
  begin
    //setting color means default display
    fFaceColorUp:=c;

    fFaceColorDisabled:=clDark;
    fBorderColor:=clActiveBorder;
    fPenColorHover:=clBlue;
    fInactiveBorderColor:=clInactiveBorder;
    fInactiveFontColor:=clInactiveCaption;


    if (c=clDefault) or (c=0) then
    begin
      //default color
      fFaceColorDown:=c;
      fFaceColorHover:=clBtnHiLight;


    end
    else
    begin
      fFaceColorDown:=DecColor(c,14);

      //there is no incColor, and -14 is not the same
      newc:=ColorToRGB(c);
      r:=Red(newc);
      g:=Green(newc);
      b:=Blue(newc);
      if r<255 then inc(r,14) else r:=255;
      if g<255 then inc(g,14) else g:=255;
      if b<255 then inc(b,14) else b:=255;
      fFaceColorHover:=RGBToColor(r,g,b);
    end;

  end;

end;


procedure TNewButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  painted:=false;

  inherited MouseMove(shift, x,y);
  if ShouldAppsUseDarkMode and (not painted) then
    repaint;
end;

procedure TNewButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited mousedown(button, shift, x,y);
  MouseIsDown:=true;
end;

procedure TNewButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, shift, x,y);
  MouseIsDown:=false;
end;

procedure TNewButton.FontChanged(Sender: TObject);
begin
  if ShouldAppsUseDarkMode then
  begin
    if self=nil then exit;

    if fcanvas<>nil then
    begin
      fCanvas.Font.BeginUpdate;
      try
        fCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
        fCanvas.Font := Font;
      finally
        fCanvas.Font.EndUpdate;
      end;
    end;

  end;
  inherited FontChanged(Sender);
end;

procedure TNewButton.PaintWindow(DC: HDC);
var
  DCChanged: boolean;
begin
  if ShouldAppsUseDarkMode then
  begin
    DCChanged := (not FCanvas.HandleAllocated) or (FCanvas.Handle <> DC);

    if DCChanged then
      FCanvas.Handle := DC;
    try
      DefaultCustomPaint;
    finally
      if DCChanged then FCanvas.Handle := 0;
    end;

    painted:=true;
  end
  else inherited paintwindow(dc);
end;

procedure TNewButton.DefaultCustomPaint;
var
  ts: TTextStyle;
  faceColor: TColor;
   GKS: TShiftState;
begin
  if fcanvas.Handle<>0 then
  begin
    if enabled then
    begin
      if self.MouseInClient then
      begin
        fborderColor:=fpenColorHover;

        if MouseIsDown then
          facecolor:=fFaceColorDown
        else
          facecolor:=fFaceColorHover;
      end
      else
        facecolor:=fFaceColorUp;
      end
    else
    begin
      facecolor:=fFaceColorDisabled;
    end;

    fcanvas.brush.style:=bsSolid;
    fcanvas.brush.color:=facecolor;
    fcanvas.Clear;

    fcanvas.pen.Width:=1;
    if enabled then
      fcanvas.pen.color:=fBorderColor
    else
      fcanvas.pen.color:=fInactiveBorderColor;

    fcanvas.Rectangle(0,0,clientwidth,clientheight);

    if enabled then
      fcanvas.font.color:=font.color
    else
      fcanvas.font.color:=fInactiveFontcolor;

    ts:=fcanvas.TextStyle;
    ts.Alignment:=taCenter;
    fcanvas.TextRect(rect(0,0,width,height),0,(height div 2)-(fcanvas.TextHeight(caption) div 2),caption, ts);


    if self.Focused then
    begin
     // fcanvas.pen.color:=
      fcanvas.DrawFocusRect(rect(2,2,width-2,height-2));
    end;

  end;
end;

procedure TNewButton.WMPaint(var Msg: TLMPaint);
begin
  if ShouldAppsUseDarkMode then
  begin
    if (csDestroying in ComponentState) or (not HandleAllocated) then exit;

    if (not fdarkmode) and (fCustomDraw or globalCustomDraw) then Include(FControlState, csCustomPaint);
    inherited WMPaint(msg);
    if (not fdarkmode) and (fCustomDraw or globalCustomDraw) then Exclude(FControlState, csCustomPaint);

  end
  else
    inherited WMPaint(msg);

end;

procedure TNewButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if ShouldAppsUseDarkMode then
  begin
    fcanvas:=TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;

    FDoubleBuffered:=true;
    setcolor(clDefault);
  end;

end;


end.

