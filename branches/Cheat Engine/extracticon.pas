unit extracticon;
{
Basicly a copy/paste from the delphi vcl with a few modifications to allow 24-bit icons
}

interface

uses windows, classes, SysUtils, graphics, consts;

procedure WriteIcon(Stream: TStream; Icon: HICON; WriteLength: Boolean);

implementation

procedure InvalidGraphic(Str: PResStringRec);
begin
  raise EInvalidGraphic.CreateRes(Str);
end;

procedure InvalidBitmap; 
begin
  InvalidGraphic(@SInvalidBitmap);
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  Colors: Integer);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then InvalidBitmap
  else if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(sizeof(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case Colors of
    2: BI.biBitCount := 1;
    3..16:
      begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;
    17..256:
      begin
        BI.biBitCount := 8;
        BI.biClrUsed := Colors;
      end;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD; Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, Colors);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * (1 shl BI.biBitCount)
    else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * BI.biClrUsed;
  ImageSize := BI.biSizeImage;
end;

procedure CheckBool(Result: Bool);
begin
  if not Result then raise exception.create('bla');
end;

function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; Colors: Integer): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
      TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    if OldPal <> 0 then SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean;
begin
  Result := InternalGetDIB(Bitmap, Palette, BitmapInfo, Bits, 0);
end;


procedure WriteIcon(Stream: TStream; Icon: HICON; WriteLength: Boolean);
var
  IconInfo: TIconInfo;
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
  CI: TCursorOrIcon;
  List: TIconRec;
  Length: Longint;
begin
  FillChar(CI, SizeOf(CI), 0);
  FillChar(List, SizeOf(List), 0);
  CheckBool(GetIconInfo(Icon, IconInfo));
  try
    InternalGetDIBSizes(IconInfo.hbmMask, MonoInfoSize, MonoBitsSize, 2);
    InternalGetDIBSizes(IconInfo.hbmColor, ColorInfoSize, ColorBitsSize, 16777216);
    MonoInfo := nil;
    MonoBits := nil;
    ColorInfo := nil;
    ColorBits := nil;
    try
      MonoInfo := AllocMem(MonoInfoSize);
      MonoBits := AllocMem(MonoBitsSize);
      ColorInfo := AllocMem(ColorInfoSize);
      ColorBits := AllocMem(ColorBitsSize);
      InternalGetDIB(IconInfo.hbmMask, 0, MonoInfo^, MonoBits^, 2);
      InternalGetDIB(IconInfo.hbmColor, 0, ColorInfo^, ColorBits^, 16777216);
      if WriteLength then
      begin
        Length := SizeOf(CI) + SizeOf(List) + ColorInfoSize +
          ColorBitsSize + MonoBitsSize;
        Stream.Write(Length, SizeOf(Length));
      end;
      with CI do
      begin
        CI.wType := RC3_ICON;
        CI.Count := 1;
      end;
      Stream.Write(CI, SizeOf(CI));
      with List, PBitmapInfoHeader(ColorInfo)^ do
      begin
        Width := biWidth;
        Height := biHeight;
        Colors := biPlanes * biBitCount;
        DIBSize := ColorInfoSize + ColorBitsSize + MonoBitsSize;
        DIBOffset := SizeOf(CI) + SizeOf(List);
      end;
      Stream.Write(List, SizeOf(List));
      with PBitmapInfoHeader(ColorInfo)^ do
        Inc(biHeight, biHeight); { color height includes mono bits }
      Stream.Write(ColorInfo^, ColorInfoSize);
      Stream.Write(ColorBits^, ColorBitsSize);
      Stream.Write(MonoBits^, MonoBitsSize);
    finally
      FreeMem(ColorInfo, ColorInfoSize);
      FreeMem(ColorBits, ColorBitsSize);
      FreeMem(MonoInfo, MonoInfoSize);
      FreeMem(MonoBits, MonoBitsSize);
    end;
  finally
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
  end;
end;


end.

