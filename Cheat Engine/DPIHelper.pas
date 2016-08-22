unit DPIHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, Graphics, forms;

procedure AdjustSpeedButtonSize(sb: TSpeedButton);

implementation

procedure AdjustSpeedButtonSize(sb: TSpeedButton);
const
  designtimedpi=96;
//  designtimedpi=50;
var
  bm: TBitmap;
  ng: integer;
begin
  if screen.PixelsPerInch<>designtimedpi then
  begin
    sb.Transparent:=false;
    sb.Glyph.Transparent:=false;
    ng:=sb.NumGlyphs;

    bm:=TBitmap.Create;
    bm.Assign(sb.Glyph);

    bm.width:=scalex(sb.Glyph.Width, designtimedpi);
    bm.height:=scaley(sb.Glyph.Height, designtimedpi);
    bm.Canvas.StretchDraw(rect(0,0, bm.width, bm.height),sb.Glyph);

    sb.Width:=scalex(sb.Width, designtimedpi);
    sb.Height:=scaley(sb.Height, designtimedpi);
    bm.TransparentColor:=0;
    bm.TransparentMode:=tmAuto;

    sb.Glyph:=bm;
    sb.Glyph.Transparent:=true;
    sb.NumGlyphs:=ng;
    sb.Transparent:=true;
    bm.free;
  end;
end;

end.

