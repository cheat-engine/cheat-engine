unit DPIHelper;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, controls, comctrls, SysUtils, Buttons, Graphics, forms, StdCtrls;

procedure AdjustSpeedButtonSize(sb: TSpeedButton);
procedure AdjustToolbar(tb: TToolbar);
procedure AdjustImageList(il: TImageList);
procedure AdjustComboboxSize(cb: TComboBox; canvas: TCanvas);
procedure AdjustEditBoxSize(editbox: TCustomEdit; mintextwidth: integer);
function GetEditBoxMargins(editbox: TCustomEdit): integer;

function getDPIScaleFactor: single;

implementation

uses globals, win32proc;

const
  designtimedpi=96;

function getDPIScaleFactor: single;
begin
  result:=screen.PixelsPerInch/designtimedpi;
end;

function GetEditBoxMargins(editbox: TCustomEdit): integer;
var m: dword;
begin
  if WindowsVersion>=wvVista then
    m:=sendmessage(editbox.Handle, EM_GETMARGINS, 0,0)
  else
    m:=10;

  result:=(m shr 16)+(m and $ffff);
end;

procedure AdjustEditBoxSize(editbox: TCustomEdit; mintextwidth: integer);
var marginsize: integer;
begin
  marginsize:=GetEditBoxMargins(editbox);
  editbox.clientwidth:=mintextwidth+marginsize;
end;

procedure AdjustComboboxSize(cb: TComboBox; canvas: TCanvas);
var
  cbi: TComboboxInfo;
  i: integer;
  s: string;
  maxwidth: integer;

  w: integer;
begin
  maxwidth:=0;
  for i:=0 to cb.Items.Count-1 do
  begin
    s:=cb.Items[i];
    maxwidth:=max(maxwidth, Canvas.TextWidth(s));
  end;

  cbi.cbSize:=sizeof(cbi);
  if GetComboBoxInfo(cb.Handle, @cbi) then
  begin
    i:=maxwidth-(cbi.rcItem.Right-cbi.rcItem.Left)+4;

    w:=cb.width+i;
  end
  else
    w:=maxwidth+16;

  cb.width:=w;
  cb.Constraints.MinWidth:=w;
end;

procedure ScaleImageList(ImgList: TImageList; NewWidth, NewHeight: Integer); //from http://wiki.freepascal.org/TImageList
var
  TempImgList: TImageList;
  TempBmp1: TBitmap;
  TempBmp2: TBitmap;
  I: Integer;
begin
  TempImgList := TImageList.Create(nil);
  TempBmp1 := TBitmap.Create;
  TempBmp1.PixelFormat := pf32bit;
  TempBmp2 := TBitmap.Create;
  TempBmp2.PixelFormat := pf32bit;
  TempBmp2.SetSize(NewWidth, NewHeight);
  try
    TempImgList.Width := NewWidth;
    TempImgList.Height := NewHeight;

    for I := 0 to ImgList.Count - 1 do begin
      // Load image for given index to temporary bitmap
      ImgList.GetBitmap(I, TempBmp1);

      // Clear transparent image background
      TempBmp2.Canvas.Brush.Style := bsSolid;
      TempBmp2.Canvas.Brush.Color := TempBmp2.TransparentColor;
      TempBmp2.Canvas.FillRect(0, 0, TempBmp2.Width, TempBmp2.Height);

      // Stretch image to new size
      TempBmp2.Canvas.StretchDraw(Rect(0, 0, TempBmp2.Width, TempBmp2.Height), TempBmp1);
      TempImgList.Add(TempBmp2, nil);
    end;

    ImgList.Assign(TempImgList);
  finally
    TempImgList.Free;
    TempBmp1.Free;
    TempBmp2.Free;
  end;
end;

procedure AdjustImageList(il: TImageList);
begin
  if (screen.PixelsPerInch<>designtimedpi) then
  begin
    ScaleImageList(il, scalex(il.Height, designtimedpi), scaley(il.width,designtimedpi));
  end
  else
  begin
    ScaleImageList(il, trunc(il.Height*fontmultiplication), trunc(il.Width*fontmultiplication));
  end;

end;

procedure AdjustToolbar(tb: TToolbar);
var
  i: integer;
  originalbm: TBitmap;
  bm: Tbitmap;
begin
  if (fontmultiplication>1.0) or (screen.PixelsPerInch<>designtimedpi) then
  begin
    if (screen.PixelsPerInch<>designtimedpi) then
    begin
      tb.ButtonHeight:=scalex(tb.ButtonHeight, designtimedpi);
      ScaleImageList(timagelist(tb.Images), scalex(tb.Images.Height, designtimedpi), scaley(tb.images.Width, designtimedpi));
    end
    else
    begin
      tb.ButtonHeight:=trunc(tb.ButtonHeight*fontmultiplication);
      ScaleImageList(timagelist(tb.Images), trunc(tb.Images.Height*fontmultiplication), trunc(tb.images.Width*fontmultiplication));
    end;



    {
    for i:=0 to tb.images.Count-1 do
    begin
      originalbm:=TBitmap.Create;
      tb.images.GetBitmap(i,originalbm);

      originalbm.Transparent:=false;

      originalbm.SaveToFile('d:\bla.bmp');

      bm:=Tbitmap.create;
      bm.Assign(originalbm);



      if (screen.PixelsPerInch<>designtimedpi) then
      begin
        bm.width:=scalex(originalbm.Width, designtimedpi);
        bm.height:=scaley(originalbm.Height, designtimedpi);
      end
      else
      begin
        bm.width:=trunc(originalbm.width*fontmultiplication);
        bm.height:=trunc(originalbm.height*fontmultiplication);
      end;

      bm.SaveToFile('d:\bla1.5.bmp');

      bm.Canvas.StretchDraw(rect(0,0, bm.width, bm.height),originalbm);
      bm.SaveToFile('d:\bla2.bmp');

      tb.Images.Replace(i,bm,nil);
    end; }
  end;
end;

procedure AdjustSpeedButtonSize(sb: TSpeedButton);

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

