unit newHeaderControl; //for some reason parentfont does not work

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls,  ComCtrls, CommCtrl;

type
  TNewHeaderControl=class(THeaderControl)
  private
    darkmode: boolean;
  protected
    procedure ChildHandlesCreated; override;

  public
    procedure Paint; override;
    procedure PaintSection(Index: Integer); override;
  end;


implementation

uses betterControls, Graphics;

procedure TNewHeaderControl.Paint;
var r: trect;
begin
  inherited paint;

  if ShouldAppsUseDarkMode and darkmode then
  begin

    r:=Self.GetClientRect;

    if sections.count>0 then
      r.left:=sections[sections.count-1].Right;

    if r.left>clientwidth-1 then r.left:=clientwidth-1;   //for the last border

    canvas.brush.color:=colorset.TextBackground;
    canvas.pen.color:=colorset.ButtonBorderColor;
    canvas.FillRect(r);
    canvas.Rectangle(r);
  end;

end;

procedure TNewHeaderControl.PaintSection(Index: Integer);
var
  ARect: TRect;
  Section: THeaderSection;
  ts: TTextStyle;
begin
  if ShouldAppsUseDarkMode and darkmode then
  begin
    //draw it myself
    Section := Sections[Index];
    if not Section.Visible then Exit;

    if section.Right <= section.Left then
      exit;

    arect.Left:=section.left;
    arect.top:=0;
    arect.Bottom:=height;
    arect.Right:=section.Right;

    case section.State of
      hsNormal: canvas.brush.color:=colorset.TextBackground;
      hsHot: canvas.brush.color:=incColor(colorset.TextBackground,16);
      hsPressed: canvas.brush.color:=incColor(colorset.TextBackground,32);
    end;

    canvas.pen.color:=colorset.ButtonBorderColor;
    canvas.FillRect(arect);
    canvas.Rectangle(arect);

    ts:=canvas.TextStyle;
    ts.Alignment:=section.Alignment;
    ts.Layout:=tlCenter;
    canvas.TextRect(arect,arect.left+2,0,section.Text,ts);



  end
  else
    inherited PaintSection(index);

end;

procedure TNewHeaderControl.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode and (Parent<>nil) then
  begin
    if ShouldAppsUseDarkMode() then
      darkmode:=true;
  end;
end;

end.

