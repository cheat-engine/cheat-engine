unit newButton;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, StdCtrls, LCLType, Graphics, LMessages, Controls;

type
  TButton=class(StdCtrls.TButton)
  private
    painted: boolean;
    fCanvas: TCanvas;
    fCustomDraw: boolean;
    fOnPaint: TNotifyEvent;

  protected
    procedure DefaultCustomPaint;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property CustomDraw: boolean read fCustomDraw write fCustomDraw;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
    property Canvas: TCanvas read fCanvas;
  end;


var globalCustomDraw: boolean;

implementation

procedure TButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  painted:=false;

  inherited MouseMove(shift, x,y);
  if not painted then
    repaint;
end;

procedure TButton.FontChanged(Sender: TObject);
begin
  if self=nil then exit;

  if canvas<>nil then
  begin
    Canvas.Font.BeginUpdate;
    try
      Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
      Canvas.Font := Font;
    finally
      Canvas.Font.EndUpdate;
    end;
  end;
  inherited FontChanged(Sender);
end;

procedure TButton.PaintWindow(DC: HDC);
var
  DCChanged: boolean;
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
end;

procedure TButton.DefaultCustomPaint;
var
  ts: TTextStyle;
  faceColor: TColor;
begin
  if fcanvas.Handle<>0 then
  begin
    if enabled then
    begin
      if self.MouseInClient then
        facecolor:=clBtnHighlight
      else
        facecolor:=GetSysColor(CTLCOLOR_BTN); //  clBtnFace;
      end
    else
    begin
      facecolor:=clDark;
    end;

    fcanvas.brush.style:=bsSolid;
    fcanvas.brush.color:=facecolor;
    fcanvas.Clear;

    fcanvas.pen.Width:=1;
    if enabled then
      fcanvas.pen.color:=clActiveBorder
    else
      fcanvas.pen.color:=clInactiveBorder;

    fcanvas.Rectangle(0,0,clientwidth,clientheight);

    if enabled then
      fcanvas.font.color:=clBtnText
    else
      fcanvas.font.color:=clInactiveCaption;

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

procedure TButton.WMPaint(var Msg: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then exit;

  if fCustomDraw or globalCustomDraw then Include(FControlState, csCustomPaint);
  inherited WMPaint(msg);
  if fCustomDraw or globalCustomDraw then Exclude(FControlState, csCustomPaint);
end;

procedure TButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  fcanvas:=TControlCanvas.Create;
  //fFont:=tfont.Create;
  TControlCanvas(FCanvas).Control := Self;


end;


end.

