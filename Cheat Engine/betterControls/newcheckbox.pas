unit newCheckbox;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, StdCtrls, LCLType, Graphics, LMessages, Controls;

type
  TCheckBox=class(StdCtrls.TCheckBox)
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

uses forms;

procedure TCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  painted:=false;

  inherited MouseMove(shift, x,y);
  if not painted then
    repaint;
end;

procedure TCheckBox.FontChanged(Sender: TObject);
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

procedure TCheckBox.PaintWindow(DC: HDC);
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

procedure TCheckBox.DefaultCustomPaint;
var
  ts: TTextStyle;
  faceColor: TColor;
  r: trect;

  dpiscale: single;
begin
  if fcanvas.Handle<>0 then
  begin
    facecolor:=parent.color;

    fcanvas.brush.style:=bsSolid;
    fcanvas.brush.color:=facecolor;
    fcanvas.Clear;

    fcanvas.pen.Width:=1;
    if enabled then
      fcanvas.pen.color:=clBlack
    else
      fcanvas.pen.color:=clInactiveBorder;

    fcanvas.brush.color:=clWindow;
    dpiscale:=Screen.PixelsPerInch/96;

    fcanvas.pen.width:=trunc(1*dpiscale);
    fcanvas.Pen.JoinStyle:=pjsBevel;

    r:=rect(trunc(dpiscale)-1,trunc(3*dpiscale),(trunc(dpiscale)-1)*2+clientheight-trunc((3*dpiscale)*2),(trunc(dpiscale)-1)+clientheight-trunc((3*dpiscale)));
    fcanvas.Rectangle(r);

    case state of
      cbChecked:
      begin
        fCanvas.Pen.Width:=2;
       // fcanvas.pen.EndCap:=pecFlat;
        fCanvas.PenPos:=point(r.left+trunc(1.5*dpiscale),r.top+(r.Height div 2)-(trunc(dpiscale)-1));
        fcanvas.LineTo(r.left+(r.Width div 3),r.Bottom-trunc(4*dpiscale));
        fcanvas.LineTo(r.right-trunc(2*dpiscale),r.top+trunc(2*dpiscale));

      end;

      cbUnchecked:
      begin

      end;

      cbGrayed:
      begin
        fCanvas.Pen.Width:=1;
        fcanvas.brush.color:=fcanvas.pen.color;
        fcanvas.FillRect(r.Left+r.Width div 4, r.top+r.Height div 4, r.Right-r.width div 4-(trunc(dpiscale)-1), r.Bottom-r.height div 4-(trunc(dpiscale)-1));
      end;
    end;


    if enabled then
      fcanvas.font.color:=clBtnText
    else
      fcanvas.font.color:=clInactiveCaption;

    ts:=fcanvas.TextStyle;
    ts.Alignment:=taRightJustify;
    fcanvas.TextRect(rect(0,0,width-4,height),0,(height div 2)-(fcanvas.TextHeight(caption) div 2),caption, ts);


    if self.Focused then
      fcanvas.DrawFocusRect(rect(width-fcanvas.TextWidth(caption)-4,2,width-2,height-2));
  end;
end;

procedure TCheckBox.WMPaint(var Msg: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then exit;

  if fCustomDraw or globalCustomDraw then Include(FControlState, csCustomPaint);
  inherited WMPaint(msg);
  if fCustomDraw or globalCustomDraw then Exclude(FControlState, csCustomPaint);
end;

procedure TCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  fcanvas:=TControlCanvas.Create;
  //fFont:=tfont.Create;
  TControlCanvas(FCanvas).Control := Self;
end;


end.

