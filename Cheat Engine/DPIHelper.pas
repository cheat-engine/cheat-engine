unit DPIHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, Graphics, forms;

procedure AdjustSpeedButtonSize(sb: TSpeedButton);

implementation

uses globals;

procedure AdjustSpeedButtonSize(sb: TSpeedButton);
const
  designtimedpi=96;
//  designtimedpi=50;
var
  bm: TBitmap;
  ng: integer;
begin
  if (fontmultiplication>1.0) or (screen.PixelsPerInch<>designtimedpi) then
  begin
    sb.Transparent:=false;
    sb.Glyph.Transparent:=false;
    ng:=sb.NumGlyphs;

    bm:=TBitmap.Create;
    bm.Assign(sb.Glyph);

    if (screen.PixelsPerInch<>designtimedpi) then
    begin
      bm.width:=scalex(sb.Glyph.Width, designtimedpi);
      bm.height:=scaley(sb.Glyph.Height, designtimedpi);
    end
    else
    begin
      bm.width:=trunc(bm.width*fontmultiplication);
      bm.height:=trunc(bm.height*fontmultiplication);
    end;
    bm.Canvas.StretchDraw(rect(0,0, bm.width, bm.height),sb.Glyph);

    if (screen.PixelsPerInch<>designtimedpi) then
    begin
      sb.Width:=scalex(sb.Width, designtimedpi);
      sb.Height:=scaley(sb.Height, designtimedpi);
    end
    else
    begin
      sb.width:=trunc(sb.width*fontmultiplication);
      sb.height:=trunc(sb.height*fontmultiplication);
    end;
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

