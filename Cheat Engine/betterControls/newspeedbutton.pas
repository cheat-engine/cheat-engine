unit newSpeedButton; //for some reason parentfont does not work

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, Buttons;

type
  TNewSpeedButton=class(TSpeedButton)
  private
    darkmode: boolean;
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
    procedure SetParent(NewParent: TWinControl); override;
  public
  end;


implementation

uses betterControls, themes, Graphics;

procedure TNewSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  if ShouldAppsUseDarkMode and darkmode then
  begin
    case FState of
      bsUp: Canvas.Brush.Color := Color;
      bsDisabled: Canvas.brush.Color:= Color xor $aaaaaa;
      bsHot: Canvas.Brush.Color:=incColor(color,15); //or $aaaaaa;
      bsDown: Canvas.Brush.Color :=incColor(color,32);
    end;

    Canvas.FillRect(PaintRect);
    canvas.pen.color:=color xor $aaaaaa;
    Canvas.Rectangle(PaintRect);
  end
  else
    inherited PaintBackground(paintrect);
end;

procedure TNewSpeedButton.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(newparent);

  if (NewParent<>nil) and ShouldAppsUseDarkMode then
  begin
    darkmode:=true;
    color:=ColorSet.TextBackground;
  end;
end;

end.

