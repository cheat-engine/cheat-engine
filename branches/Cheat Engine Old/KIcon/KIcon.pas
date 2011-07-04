{ @abstract(This unit provides an advanced Windows icon management
    i.e. replacement for the Graphics.TIcon component)
  @author(Tomas Krysl (tomkrysl@quick.cz))
  @created(9 January 2005)
  @lastmod(14 July 2006)

  The purpose of the TKIcon component is to replace and expand the standard
  TIcon component provided by VCL. The TKIcon component is not based on Windows
  icon functions, but manages the icon structures by itself.

  <UL>
    <LH>Major features are:</LH>
    <LI>32-bit icons/cursors with alpha channel supported</LI>
    <LI>correct rendering in all 32-bit Windows platforms</LI>
    <LI>optional rendering of all icon/ cursors subimages</LI>
    <LI>icons/cursors can be stretched when drawn</LI>
    <LI>multiple rendering styles</LI>
    <LI>loading from file/stream, HICON, module resources, file associations</LI>
    <LI>saving to file/stream</LI>
    <LI>icon image manipulation (inserting/deleting/cropping/enlarging)</LI>
    <LI>full TPicture integration (only TPicture.Icon can't be used)</LI>
  </UL>

  Copyright © 2005-6 Tomas Krysl (tomkrysl@@quick.cz)<BR><BR>

  <B>Licence:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the native or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code.
}

unit KIcon;

interface

uses
  Windows, SysUtils, Classes, Graphics;

resourcestring
  { @exclude }
  SVCursors = 'Cursors';
  { @exclude }
  SIconAllocationError = 'Error while allocating icon data';
  { @exclude }
  SIconBitmapError = 'Invalid icon bitmap handles';
  { @exclude }
  SIconFormatError = 'Invalid icon format';
  { @exclude }
  SIconGDIError = 'GDI object could not be created';
  { @exclude }
  SIconResourceError = 'Invalid icon resource';
  { @exclude }
  SIconIndexError = 'Invalid icon resource index';
  { @exclude }
  SIconInvalidModule = 'Invalid module or no icon resources';
  { @exclude }
  SIconResizingError = 'Error while resizing icon';
  { @exclude }
  SIconAssocResolveError = 'Error while resolving associated icon';

type
  { @abstract(Icon file header)
    <UL>
    <LH>Members:</LH>
    <LI><I>idReserved</I> - always 0</LI>
    <LI><I>idType</I> - 1=icon, 2=cursor</LI>
    <LI><I>idCount</I> - total number of icon images in file</LI>
    </UL>
  }
  TIconHeader = packed record
    idReserved: Word;
    idType: Word;
    idCount: Word;
  end;

  { Pointer to the icon file header structure }
  PIconHeader = ^TIconHeader;

  { @abstract(Helper structure identifying attributes that are different for
    icons and cursors)
    <UL>
    <LH>Members:</LH>
    <LI><I>wPlanes</I> - for icons: amount of image planes - I think that this is always 1</LI>
    <LI><I>wBitCount</I> - for icons: image color resolution</LI>
    <LI><I>wX</I> - for cursors: hot spot horizontal coordinate</LI>
    <LI><I>wY</I> - for cursors: hot spot vertical coordinate</LI>
    </UL>
  }
  TIconCursorDirInfo = packed record
    case Integer of
    0: (
      wPlanes: Word;
      wBitCount: Word;
      );
    1: (
      wX: Word;
      wY: Word;
      );
  end;

  { @abstract(Icon/cursor directory entry. This structure decribes each
    icon/cursor image. These structures describing all images immediately follow
    the @link(TIconHeader) structure in the icon file. After these the bitmap data
    for all images are stored (TBitmapInfoHeader, palette data, bitmap bits - XOR, AND).)
    <UL>
    <LH>Members:</LH>
    <LI><I>Width</I> - image width</LI>
    <LI><I>Height</I> - image height</LI>
    <LI><I>ColorCount</I> - number of entries in palette table</LI>
    <LI><I>Reserved</I> - not used</LI>
    <LI><I>Info</I> - different for icons/cursors</LI>
    <LI><I>dwBytesInRes</I> - total number bytes in the image including
      pallette data, XOR bits, AND bits and bitmap info header</LI>
    <LI><I>dwImageOffset</I> - position of image as offset from the beginning of file</LI>
    </UL>
  }
  TIconCursorDirEntry = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
    Info: TIconCursorDirInfo;
    dwBytesInRes: Longint;
    dwImageOffset: Longint;
  end;

  { Pointer to the icon/cursor directory entry }
  PIconCursorDirEntry = ^TIconCursorDirEntry;

  { Helper structure to typecast cursor hot spot coordinates }
  TCursorHotSpot = packed record
    xHotSpot: Word;
    yHotSpot: Word;
  end;

  { Pointer to the cursor hot spot structure }
  PCursorHotSpot = ^TCursorHotSpot;

  { Helper structure for cursor specific data in resource file }
  TCursorDir = packed record
    Width: Word;
    Height: Word;
  end;

  { Helper structure for icon specific data in resource file }
  TIconResdir = packed record
    Width: Byte;
    Height: Byte;
    ColorCount: Byte;
    Reserved: Byte;
  end;

  { Helper structure merging icon and cursor specific data }
  TIconCursorInfo = packed record
    case Integer of
      0: (Icon: TIconResdir);
      1: (Cursor: TCursorDir);
  end;

  { @abstract(Icon/cursor directory entry as found in resource files)
    <UL>
    <LH>Members:</LH>
    <LI><I>Info</I> - structure that merges icon/cursor specific data</LI>
    <LI><I>wPlanes</I> - not used = 0</LI>
    <LI><I>wBitCount</I> - not used = 0</LI>
    <LI><I>dwBytesInRes</I> - total number of bytes in the image including
      pallette data, XOR bits, AND bits and bitmap info header</LI>
    <LI><I>wEntryName</I> - icon/cursor entry name. This number identifies the
      particular icon image in a resource file (images are stored under ICONENTRY
      key)</LI>
    </UL>
  }
  TIconCursorDirEntryInRes = packed record
    Info: TIconCursorInfo;
    wPlanes: Word;
    wBitCount: Word;
    dwBytesInRes: Longint;
    wEntryName: Word;
  end;

  { Pointer to the icon/cursor resource file directory entry }
  PIconCursorDirEntryInRes = ^TIconCursorDirEntryInRes;

  { Helper structure to access resource data }
  TIconCursorInRes = packed record
    IH: TIconHeader;
    Entries: array [0..MaxInt div SizeOf(TIconCursorDirEntryInRes) - 2] of TIconCursorDirEntryInRes;
  end;

  { Pointer to the helper structure }
  PIconCursorInRes = ^TIconCursorInRes;

  { Controls how the image should be aligned when they are beeing resized }
  TAlignStyle = (
    { image remains aligned to the top-left corner }
    asNone,
    { image will be centered within the new boundary rectangle }
    asCenter
  );

  { Specifies the width and height of an icon or icon image }
  TDimension = record
    Width,
    Height: Integer;
  end;

  { @abstract(Specifies the GDI handles for one icon/cursor image)
    <UL>
    <LH>Members:</LH>
    <LI><I>hXOR</I> - handle to the color bitmap - icon image</LI>
    <LI><I>hAND</I> - handle to the monochrome bitmap - icon image mask</LI>
    </UL>
  }
  THandles = record
    hXOR,
    hAND: HBITMAP;
  end;

  { @abstract(Represents the internal data structure describing each icon/cursor image)
    <UL>
    <LH>Members:</LH>
    <LI><I>Width</I> - image width</LI>
    <LI><I>Height</I> - image height</LI>
    <LI><I>Bpp</I> - image color resolution</LI>
    <LI><I>BytesInRes</I> - total image data size</LI>
    <LI><I>HotSpot</I> - hot spot for a cursor</LI>
    <LI><I>iXOR</I> - pointer to the color bitmap info header + palette</LI>
    <LI><I>iXORSize</I> - size of iXOR data</LI>
    <LI><I>pXOR</I> - pointer to the color bitmap bits</LI>
    <LI><I>pXORSize</I> - size of pXOR data</LI>
    <LI><I>hXOR</I> - handle to the color bitmap - is always a DIB section</LI>
    <LI><I>pAND</I> - pointer to the monochrome (mask) bitmap bits</LI>
    <LI><I>pANDSize</I> - size of pAND data</LI>
    <LI><I>hAND</I> - handle to the monochrome bitmap - is always a DIB section</LI>
    </UL>
  }
  TIconData = record
    Width: Integer;
    Height: Integer;
    Bpp: Integer;
    BytesInRes: Integer;
    HotSpot: TPoint;
    iXOR: PBitmapInfo;
    iXORSize: Integer;
    pXOR: Pointer;
    pXORSize: Integer;
    hXOR: HBITMAP;
    pAND: Pointer;
    pANDSize: Integer;
    hAND: HBITMAP;
  end;

  { Pointer to the internal image description structure }
  PIconData = ^TIconData;

  { Specifies how the icon image(s) should be rendered. This feature can be used
    along with the MaskFromColor method to implement a ‘color picker’ for a new mask construction. }
  TIconDrawStyle = (
    { paint normally }
    idsNormal,
    { paint without applying the mask - color bitmap only }
    idsNoMask,
    { paint only mask - monochrome bitmap only }
    idsMaskOnly
  );

  { Main class }
  TKIcon = class(TGraphic)
  private
    FAlignStyle: TAlignStyle;
    FBpp: Integer;
    FCreating: Boolean;
    FCurrentIndex: Integer;
    FCursor: Boolean;
    FDisplayAll: Boolean;
    FDisplayHorz: Boolean;
    FIconCount: Integer;
    FIconData: array of TIconData;
    FIconDrawStyle: TIconDrawStyle;
    FInHandleBpp: Integer;
    FInHandleFullAlpha: Boolean;
    FMaxHeight: Integer;
    FMaxWidth: Integer;
    FOptimalIcon: Boolean;
    FOverSizeWeight: Single;
    FRequestedSize: TDimension;
    FSpacing: Integer;
    FStretchEnabled: Boolean;
    function GetDimensions(Index: Integer): TDimension;
    function GetHandles(Index: Integer): THandles;
    function GetHeights(Index: Integer): Integer;
    function GetHotSpot(Index: Integer): TPoint;
    function GetIconData(Index: Integer): TIconData;
    function GetWidths(Index: Integer): Integer;
    procedure SetCurrentIndex(Value: Integer);
    procedure SetDimensions(Index: Integer; Value: TDimension);
    procedure SetDisplayAll(Value: Boolean);
    procedure SetDisplayHorz(Value: Boolean);
    procedure SetHandles(Index: Integer; Value: THandles);
    procedure SetHeights(Index: Integer; Value: Integer);
    procedure SetHotSpot(Index: Integer; Value: TPoint);
    procedure SetInHandleBpp(Value: Integer);
    procedure SetIconDrawStyle(Value: TIconDrawStyle);
    procedure SetOptimalIcon(Value: Boolean);
    procedure SetOverSizeWeight(Value: Single);
    procedure SetRequestedSize(Value: TDimension);
    procedure SetSpacing(Value: Integer);
    procedure SetStretchEnabled(Value: Boolean);
    procedure SetWidths(Index: Integer; Value: Integer);
  protected
    { Overriden method - see Delphi help. Calls @link(Update) method. }
    procedure Changed(Sender: TObject); override;
    { Overriden method - see Delphi help. }
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    { Overriden method - see Delphi help. }
    function GetEmpty: Boolean; override;
    { Overriden method - see Delphi help. }
    function GetHeight: Integer; override;
    { Overriden method - see Delphi help. }
    function GetWidth: Integer; override;
    { Copies the bitmaps stored in Handles to the icon image identified by Index.
      If OrigBpp is True, the color resolution for the color bitmap remains unchanged,
      otherwise the value of InHandleBpp will be used. }
    procedure LoadHandles(Index: Integer; const Handles: THandles; OrigBpp: Boolean);
    { Overriden method - see Delphi help. }
    procedure SetHeight(Value: Integer); override;
    { Overriden method - see Delphi help. }
    procedure SetTransparent(Value: Boolean); override;
    { Overriden method - see Delphi help. }
    procedure SetWidth(Value: Integer); override;
    { Updates @link(MaxWidth), @link(MaxHeight) and @link(CurrentIndex)
      properties accordingly. }
    procedure Update; dynamic;
    { Resizes an icon image identified by Index to new dimensions stored in Value.
      The AlignStyle property controls the image alignment within the new rectangle. }
    procedure UpdateDim(Index: Integer; Value: TDimension);
  public
    { Overriden method - see Delphi help. }
    constructor Create; override;
    { Overriden method - see Delphi help. }
    destructor Destroy; override;
    { Adds a new image to the end of the internal image list. You should always
      specify valid color and mask bitmap handles else an exception will occur. }
    procedure Add(const Handles: THandles);
    { Overriden method - see Delphi help. }
    procedure Assign(Source: TPersistent); override;
    { Clears all images so that the instance contains no icon/cursor. }
    procedure Clear; dynamic;
    { Creates an icon handle for use with Win32 API icon functions. The image
      identified by Index will be used for this handle. If DisplayAll is False
      and Index is out of range, CurrentIndex will be used instead. }
    function CreateHandle(Index: Integer): HICON;
    { Deletes an image identified by Index from the internal image list. }
    procedure Delete(Index: Integer);
    { Inserts an image at the position identified by Index into the internal
      image list. The existing images will be preserved and shifted accordingly. }
    procedure Insert(Index: Integer; const Handles: THandles);
    { Overriden method - see Delphi help. Does nothing for icons/cursors. }
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    { Loads the icon from the module associated with the file identified by FileName
      (DefaultIcon registry key). If no association can be found for the file,
      an exception will be raised and the function will try to load FileName
      as if it was a module itself. }
    procedure LoadFromAssocFile(const FileName: string);
    { Loads the icon from the module associated with the file extension identified
      by Extension (DefaultIcon registry key). The Extension parameter should
      contain the leading period ('.'). If no association can be found for that
      extension, an exception will be triggered. }
    procedure LoadFromAssocExtension(const Extension: string);
    { Loads the icon from Win32 API icon handle. Please keep in mind that icon bitmaps
      can't be loaded as DIBs because they are already converted to DDBs when
      accessible through HICON. So it is impossible to load the icon in it's
      native format (e.g. as stored in an *.ico file) from HICON. This function
      has been introduced only to complete the loading schemes of this class
      and you should rather use another LoadFrom... methods. The behavior of this
      function can be controlled via the InHandleBpp and InHandleFullAlpha properties.
      It is not recommended to use this function in new projects. }
    procedure LoadFromHandle(Handle: HICON);
    { Loads the icon from resources of a module identified by ModuleName.
      A valid icon resource must be specified by ID, otherwise
      an exception occurs. This function uses the LoadLibrary API function, so
      it is recommended to use the LoadFromResourceX functions to load multiple
      icons from the same module. ID is of type Word so it can’t exceed 65535. }
    procedure LoadFromModule(const ModuleName: string; ID: Word); overload;
    { Does the same thing, but with resource ID specified as string. Let's suppose
      ID = 123. Here you can pass it as a string '#123'. }
    procedure LoadFromModule(const ModuleName, ResName: string); overload;
    { This function does the same as @link(LoadFromModule), but the icon resource
      is specified by index here. The index stands for the n-th icon stored
      in the module resources. So, LoadFromModule('dummy.exe', 'MAINICON') would
      produce the same results as LoadFromModuleByIndex('dummy.exe', 0),
      provided 'MAINICON' is the first icon resource in 'dummy.exe'. }
    procedure LoadFromModuleByIndex(const ModuleName: string; Index: Integer);
    { Loads the icon from resources of a module instance identified by Instance.
      Further behavior corresponds to @link(LoadFromModule) with resource ID
      specified as integer. }
    procedure LoadFromResource(Instance: HINST; ID: Word); overload;
    { Loads the icon from resources of a module instance identified by Instance.
      Further behavior corresponds to @link(LoadFromModule) with resource ID
      specified as string. }
    procedure LoadFromResource(Instance: HINST; const ResName: string); overload;
    { Loads the icon from resources of a module instance identified by Instance.
      Further behavior corresponds to @link(LoadFromModuleByIndex). }
    procedure LoadFromResourceByIndex(Instance: HINST; Index: Integer);
    { This function is somehow special. It loads the icon from module resource raw data
      that has been directly stored into a stream. When storing a resource into a stream,
      write the raw data of the main resource descriptor first (RT_GROUP_ICON,
      RT_GROUP_CURSOR) followed by the raw data of all image resource
      (RT_ICON, RT_CURSOR). Set the stream’s position to the beginning of the raw data
      (equals to zero in most cases) before passing this stream into this function. }
    procedure LoadFromResourceStream(Stream: TStream);
    { Loads the icon from the stream. Parses the *.ico file structure.
      An overriden method. }
    procedure LoadFromStream(Stream: TStream); override;
    { Makes it possible to create a new mask bitmap for the image identified by Index.
      The new monochrome mask bitmap will be created from the color bitmap.
      Pixels of the color bitmap that match Color will be masked by the new mask,
      other pixels will be unmasked. If the Color parameter contains alpha channel,
      you should set HasAlpha to True to perform comparison with the alpha channel.
      Otherwise, only the red, green and blue channels will be compared. }
    procedure MaskFromColor(Index: Integer; Color: TColor; HasAlpha: Boolean = False);
    { Overriden method - see Delphi help. Does nothing for icons/cursors. }
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    { Saves the icon to the stream. Assembles the *.ico file structure. An overriden method. }
    procedure SaveToStream(Stream: TStream); override;
    { Controls the icon image resizing which is performed by the UpdateDim method. }
    property AlignStyle: TAlignStyle read FAlignStyle write FAlignStyle;
    { Specifies the index of the currently displayed icon image.
      If no image is loaded (no icon), the value of CurrentIndex is -1. }
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    { Indicates whether the instance of this class represents a cursor (True) or an icon (False). }
    property Cursor: Boolean read FCursor write FCursor;
    { Specifies whether all icon images (True) or a single subimage should be
      drawn (False). When True, all available icon images will be rendered. }
    property DisplayAll: Boolean read FDisplayAll write SetDisplayAll;
    { Specifies how the images should be drawn when @link(DisplayAll) is True.
      If True, the images will be drawn horizontally aligned. If False,
      the images will be drawn vertically aligned. }
    property DisplayHorz: Boolean read FDisplayHorz write SetDisplayHorz;
    { Makes it possible to read/modify the size of an icon image. }
    property Dimensions[Index: Integer]: TDimension read GetDimensions write SetDimensions;
    { Makes it possible to read/modify icon image bitmaps (color and mask bitmap).
      Bitmaps that you pass will be copied and remain unchanged. When reading
      original bitmap handles are returned and thus must not be modified or released. }
    property Handles[Index: Integer]: THandles read GetHandles write SetHandles;
    { Makes it possible to read/modify the height of an icon image. }
    property Heights[Index: Integer]: Integer read GetHeights write SetHeights;
    { For a cursor, this property contains the hot spots for all cursor images. }
    property HotSpot[Index: Integer]: TPoint read GetHotSpot write SetHotSpot;
    { Returns the number of images found in this instance. }
    property IconCount: Integer read FIconCount;
    { Makes it possible to read the internal data structure of each icon image.
      A copy of the structure is returned but the pointers or handles are original
      (no copies are created) and thus must not be modified or released. }
    property IconData[Index: Integer]: TIconData read GetIconData;
    { Affects the icon image rendering. }
    property IconDrawStyle: TIconDrawStyle read FIconDrawStyle write SetIconDrawStyle;
    { Specifies the color resolution a DIB should have after converted from a DDB
      that has been passed to the LoadHandles method. }
    property InHandleBpp: Integer read FInHandleBpp write SetInHandleBpp;
    { Determines whether a DIB with 32 bits per pixel should have full visibility
      (alpha channel of each pixel set to 0xFF) after converted from a DDB
      that has been passed to the LoadHandles method. The alpha channel values will
      be only set to 0xFF when the current alpha channel of every pixel is zero. }
    property InHandleFullAlpha: Boolean read FInHandleFullAlpha write FInHandleFullAlpha;
    { Returns the height of the image that has the maximum height of all icon images.
      When @link(DisplayAll) is True and @link(DisplayHorz) is False, returns the
      total height of all images and spaces between them (specified by @link(Spacing)). }
    property MaxHeight: Integer read FMaxHeight;
    { Returns the width of the image that has the maximum width of all icon images.
      When both @link(DisplayAll) and @link(DisplayHorz) is True, returns the
      total width of all images and spaces between them (specified by @link(Spacing)). }
    property MaxWidth: Integer read FMaxWidth;
    { This property applies only when DisplayAll is False. It determines whether
      the icon image corresponding to the RequestedSize property and the current
      display mode color resolution (True) or the subimage specified by CurrentIndex
      (False) should be displayed. }
    property OptimalIcon: Boolean read FOptimalIcon write SetOptimalIcon;
    { Controls the decision threshold for the optimal image when OptimalIcon is True.
      The bigger the value is, the less is the probability an subimage greater than
      RequestedSize will be selected. This value is big enough by default so that
      almost always a smaller image will be selected if none with the exact size is found. }
    property OverSizeWeight: Single read FOverSizeWeight write SetOverSizeWeight;
    { Specifies the preferred image size when OptimalIcon is True.
      When OverSizeWeight is small, a greater subimage may be often selected. }
    property RequestedSize: TDimension read FRequestedSize write SetRequestedSize;
    { Specifies the spacing between icon images when @link(DisplayAll) is True. }
    property Spacing: Integer read FSpacing write SetSpacing;
    { Specifies whether icon images can be stretched when drawn. This property
      was introduced perhaps only for backward compatibility with Graphics.TIcon. }
    property StretchEnabled: Boolean read FStretchEnabled write SetStretchEnabled;
    { Makes it possible to read/modify the width of an icon image. }
    property Widths[Index: Integer]: Integer read GetWidths write SetWidths;
  end;

  { This class is necessary because of the TPicture streaming. }
  TIcon = class(TKIcon);

{ Returns the structure containing hXOR and hAND bitmaps. }
function MakeHandles(hXOR, hAND: HBITMAP): THandles;

{ Returns the total number of resources of a type specified by ResType
  in a module identified by Instance. }
function GetModuleResourceCount(Instance: HINST; ResType: PAnsiChar): Integer;

{ Returns the total number of HW-independent icon resources
  in a module identified by Instance. }
function GetModuleIconCount(Instance: HINST): Integer; overload;

{ Returns the total number of HW-independent icon resources
  in a module identified by ModuleName. }
function GetModuleIconCount(const ModuleName: string): Integer; overload;

implementation

uses
  Consts, Registry, JclGraphics, JclGraphUtils, JclSysInfo;

type
  PColorRec = ^TColorRec;
  TColorRec = packed record
    R, G, B, Alpha: Byte;
  end;

  TMaskBitmapInfo = packed record
    Header: TBitmapInfoHeader;
    Black,
    White: TRGBQuad;
  end;

procedure Error(const S: string);
begin
  raise Exception.Create(S);
end;                  

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then Error(SIconGDIError);
  Result := Value;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function DivUp(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor > 0 then
    Result := Dividend div Divisor + 1
  else
    Result := Dividend div Divisor;
end;

function K_RGBToBGR(Value: TColor): TColor;
var
  B: Byte;
begin
  Result := Value;
  B := PColorRec(@Value).B;
  PColorRec(@Result).B := PColorRec(@Result).R;
  PColorRec(@Result).R := B;
end;

procedure FreeSubimage(PID: PIconData);
begin
  FreeMem(PID.iXOR);
  if PID.hXOR <> 0 then DeleteObject(PID.hXOR);
  if PID.hAND <> 0 then DeleteObject(PID.hAND);
  FillChar(PID^, SizeOf(TIconData), 0);
end;

function CalcByteWidth(Width, Bpp: Integer): Integer;
begin
  Result := DivUp(Width * Bpp, SizeOf(LongWord) shl 3) * SizeOf(LongWord);
end;

function CalcBitmapSize(Width, Height, Bpp: Integer): Integer;
begin
  Result := CalcByteWidth(Width, Bpp) * Height;
end;

procedure CalcByteWidths(Width, Bpp: Integer; var XORWidth, ANDWidth: Integer);
begin
  XORWidth := CalcByteWidth(Width, Bpp);
  ANDWidth := CalcByteWidth(Width, 1);
end;

procedure CalcBitmapSizes(Width, Height, Bpp: Integer; var XORSize, ANDSize: Integer);
begin
  XORSize := CalcBitmapSize(Width, Height, Bpp);
  ANDSize := CalcBitmapSize(Width, Height, 1);
end;

function GetPaletteSize(Bpp: Integer): Integer;
begin
  if Bpp <= 8 then
    Result := 1 shl Bpp
  else
    Result := 0;
end;

procedure QueryBitmapBits(DC: HDC; hBmp: HBITMAP; var Bits: Pointer; var Size: Integer);
var
  BInfo: Windows.TBitmap;
  BI: TBitmapInfo;
begin
  GetObject(hBmp, SizeOf(Windows.TBitmap), @BInfo);
  Size := CalcBitmapSize(BInfo.bmWidth, BInfo.bmHeight, BInfo.bmBitsPixel);
  GetMem(Bits, Size);
  FillChar(BI, SizeOf(TBitmapInfo), 0);
  with BI.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := BInfo.bmWidth;
    biHeight := BInfo.bmHeight;
    biPlanes := 1;
    biBitCount := BInfo.bmBitsPixel;
    biCompression := BI_RGB;
  end;
  GetDIBits(DC, hBmp, 0, BInfo.bmHeight, Bits, BI, DIB_RGB_COLORS);
end;

procedure CreateColorInfo(Width, Height, Bpp: Integer; var BI: PBitmapInfo; var InfoSize: Integer);
begin
  InfoSize := SizeOf(TBitmapInfoHeader) + GetPaletteSize(Bpp) * SizeOf(TRGBQuad);
  GetMem(BI, InfoSize);
  FillChar(BI^, InfoSize, 0);
  with BI.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := Bpp;
  end;
end;

procedure CreateMaskInfo(Width, Height: Integer; var BIMask: TMaskBitmapInfo);
begin
  FillChar(BIMask, SizeOf(TMaskBitmapInfo), 0);
  with BIMask.Header do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 1;
  end;
  Cardinal(BIMask.Black) := clBlack;
  Cardinal(BIMask.White) := clWhite;
end;

function CreateMonochromeBitmap(Width, Height: Integer): HBITMAP;
begin
  Result := GDICheck(CreateBitmap(Width, Height, 1, 1, nil));
end;

procedure MaskOrBitBlt(ACanvas: TCanvas; X, Y, Width, Height: Integer;
  DC_XOR, DC_AND: HDC; BM_XOR, BM_AND: HBITMAP;
  XORBits: PColor32Array; XORSize: Integer;
  ANDBits: PByteArray; ANDSize: Integer;
  Bpp: Integer; Style: TIconDrawStyle);
var
  I, J, K, L, LAnd: Integer;
  ByteMask: Byte;
  FreeBits: Boolean;
  PSrc, PDest: PColor32Array;
  Q: PByteArray;
  Ps, Pd: PColor32;
  BMSrc, BMDest: TJclBitmap32;
  R, D: TRect;
begin
  if Style <> idsMaskOnly then
  begin
    BMSrc := TJclBitmap32.Create;
    BMDest := TJclBitmap32.Create;
    try
      BMSrc.SetSize(Width, Height);
      R := Rect(0, 0, Width, Height);
      D := R;
      OffsetRect(D, X, Y);
      if Bpp = 32 then
      begin // perform alphablend
        if XORBits = nil then
        begin
          QueryBitmapBits(DC_XOR, BM_XOR, Pointer(XORBits), XORSize);
          FreeBits := True;
        end else
          FreeBits := False;
        try
          BMSrc.Draw(R, D, ACanvas.Handle);
          try
            for I := 0 to Height - 1 do
            begin
              Ps := @XORBits[(Height - I - 1) * Width];
              Pd := @BMSrc.Bits[I * Width];
              BlendLine(Ps, Pd, Width);
            end
          finally
            EMMS;
          end;
        finally
          if FreeBits then FreeMem(XORBits);
        end;
      end else
        BMSrc.Draw(R, R, DC_XOR);
      if Style = idsNormal then
      begin
        BMDest.SetSize(Width, Height);
        BMDest.Draw(R, D, ACanvas.Handle);
        if ANDBits = nil then
        begin
          QueryBitmapBits(DC_XOR, BM_AND, Pointer(ANDBits), ANDSize);
          FreeBits := True;
        end else
          FreeBits := False;
        try
          PSrc := BMSrc.Bits;
          PDest := BMDest.Bits;
          LAnd := CalcByteWidth(Width, 1);
          Q := ANDBits;
          Inc(Cardinal(Q), ANDSize - LAnd);
          K := 0;
          for I := 0 to Height - 1 do
          begin
            ByteMask := $80;
            for J := 0 to Width - 1 do
            begin
              L := J shr 3;
              if Q[L] and ByteMask <> 0 then
                PSrc[K] := PDest[K];
              asm
                ror ByteMask, 1
              end;
              Inc(K);
            end;
            Dec(Cardinal(Q), LAnd);
          end;
        finally
          if FreeBits then FreeMem(ANDBits);
        end;
      end;
      BMSrc.DrawTo(ACanvas.Handle, X, Y);
    finally
      BMDest.Free;
      BMSrc.Free;
    end;
  end else
  begin
    if DC_AND = 0 then
    begin
      DC_AND := CreateCompatibleDC(ACanvas.Handle);
      try
        SelectObject(DC_AND, BM_AND);
        BitBlt(ACanvas.Handle, X, Y, Width, Height, DC_AND, 0, 0, SrcCopy);
      finally
        DeleteDC(DC_AND);
      end;
    end else
      BitBlt(ACanvas.Handle, X, Y, Width, Height, DC_AND, 0, 0, SrcCopy);
  end;
end;

procedure FillAlphaIfNone(Pixels: PColor32Array; Size: Integer; Alpha: Byte);
var
  I: Integer;
  C: TColor32;
begin
  Size := Size shr 2;
  for I := 0 to Size - 1 do
    if Pixels[I] and $FF000000 <> 0 then
      Exit; // bitmap has a nonempty alpha channel, don't fill
  C := Alpha shl 24;
  for I := 0 to Size - 1 do
    Pixels[I] := Pixels[I] or C;
end;

function MakeHandles(hXOR, hAND: HBITMAP): THandles;
begin
  Result.hXOR := hXOR;
  Result.hAND := hAND;
end;

function GetModuleResourceCount(Instance: HINST; ResType: PAnsiChar): Integer;

  function EnumIcons(hModule: HINST; lpType, lpName: PAnsiChar; dwParam: DWORD): BOOL; stdcall;
  begin
    Inc(PInteger(dwParam)^);
    Result := True;
  end;

begin
  Result := 0;
  EnumResourceNames(Instance, ResType, @EnumIcons, DWORD(@Result));
end;

function GetModuleIconCount(Instance: HINST): Integer;
begin
  Result := GetModuleResourceCount(Instance, RT_GROUP_ICON);
end;

function GetModuleIconCount(const ModuleName: string): Integer;
var
  Module: HINST;
begin
  Result := 0;
  Module := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Module <> 0 then
  begin
    try
      Result := GetModuleIconCount(Module);
    finally
      FreeLibrary(Module);
    end;
  end;
end;

{ TKIcon }

constructor TKIcon.Create;
begin
  inherited Create;
  FCreating := True;
  try
    Transparent := True; // we are not in Graphics.pas...
  finally
    FCreating := False;
  end;
  FAlignStyle := asCenter;
  FCursor := False;
  FDisplayAll := False;
  FIconDrawStyle := idsNormal;
  FInHandleBpp := 0;
  FInHandleFullAlpha := True;
  FIconData := nil;
  FOptimalIcon := True;
  FOverSizeWeight := 1000.0; // virtually always selects a lower resolution image
  FRequestedSize.Width := 32;
  FRequestedSize.Height := 32;
  FSpacing := 2;
  FStretchEnabled := True;
  Clear;
end;

destructor TKIcon.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TKIcon.Add(const Handles: THandles);
begin
  Inc(FIconCount);
  SetLength(FIconData, FIconCount);
  FillChar(FIconData[FIconCount - 1], SizeOf(TIconData), 0);
  LoadHandles(FIconCount - 1, Handles, True);
end;

procedure TKIcon.Assign(Source: TPersistent);
var
  MS: TMemoryStream;
begin
  if (Source = nil) or (Source is TKIcon) then
  begin
    Clear;
    if Source <> nil then
    begin
      FAlignStyle := TKIcon(Source).AlignStyle;
      FCursor := TKIcon(Source).Cursor;
      FDisplayAll := TKIcon(Source).DisplayAll;
      FIconDrawStyle := TKIcon(Source).IconDrawStyle;
      FInHandleBpp := TKIcon(Source).InHandleBpp;
      FInHandleFullAlpha := TKIcon(Source).InHandleFullAlpha;
      FOptimalIcon := TKIcon(Source).OptimalIcon;
      FOverSizeWeight := TKIcon(Source).OverSizeWeight;
      FRequestedSize := TKIcon(Source).RequestedSize;
      FSpacing := TKIcon(Source).Spacing;
      FStretchEnabled := TKIcon(Source).StretchEnabled;
      if not TKIcon(Source).Empty then
      begin
        MS := TMemoryStream.Create;
        try
          TKIcon(Source).SaveToStream(MS);
          MS.Position := 0;
          LoadFromStream(MS);
          FCurrentIndex := TKIcon(Source).CurrentIndex;
        finally
          MS.Free;
        end;
      end else
        Changed(Self);
    end else
      Changed(Self);
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TKIcon.Changed(Sender: TObject);
begin
  Update;
  inherited;
end;

procedure TKIcon.Clear;
var
  I: Integer;
begin
  if FIconData <> nil then
  begin
    for I := 0 to FIconCount - 1 do
      FreeSubimage(@FIconData[I]);
    FIconData := nil;
  end;
  FIconCount := 0;
  Update;
end;

function TKIcon.CreateHandle(Index: Integer): HICON;
var
  ABpp, ANDSize, XORSize: Integer;
  PID: PIconData;
  PBI: PBitmapInfo;
  DC: HDC;
  hBmp: HBITMAP;
  ANDBits, XORBits: Pointer;
begin
  Result := 0;
  if FIconData <> nil then
  begin
    DC := GetDC(0);
    try
      ABpp := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if ABpp <> FBpp then
        Update;
      if FDisplayAll then
      begin
        if (Index < 0) or (Index >= FIconCount) then
          Index := 0;
      end
      else if (Index < 0) or (Index >= FIconCount) then
        Index := FCurrentIndex;
      PID := @FIconData[Index];
      CalcBitmapSizes(PID.Width, PID.Height, FBpp, XORSize, ANDSize);
      GetMem(XORBits, XORSize);
      try
        GetMem(ANDBits, XORSize);
        try
          PBI := PID.iXOR;
          hBmp := GDICheck(CreateDIBitmap(DC, PBI.bmiHeader, CBM_INIT, PID.pXOR, PBI^, DIB_RGB_COLORS));
          try
            GetBitmapBits(hBmp, XORSize, XORBits); // obsolete, but the only that works fine...
            GetBitmapBits(PID.hAND, ANDSize, ANDbits);
            Result := CreateIcon(HInstance, PID.Width, PID.Height, 1, FBpp, ANDBits, XORBits);
          finally
            if hBmp <> 0 then DeleteObject(hBmp);
          end;
        finally
          FreeMem(ANDBits);
        end;
      finally
        FreeMem(XORBits);
      end;
    finally
      ReleaseDC(0, DC);
    end;
  end
end;

procedure TKIcon.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    FreeSubimage(@FIconData[Index]);
    for I := Index + 1 to FIconCount - 1 do
      FIconData[I - 1] := FIconData[I];
    Dec(FIconCount);
    SetLength(FIconData, FIconCount);
    Changed(Self);
  end;
end;

procedure TKIcon.Draw(ACanvas: TCanvas; const Rect: TRect);

  procedure Display(const P, WH: TPoint; Index: Integer);
  var
    ID: TIconData;
    Stretch: Boolean;
    DC, DC_XOR, DC_AND: HDC;
    BM_XOR, BM_AND: HBITMAP;
    Obj, Obj_XOR, Obj_AND: HGDIObj;
  begin
    if (Index >= 0) and (Index < FIconCount) then
    begin
      ID := FIconData[Index];
      Stretch := FStretchEnabled and ((WH.X <> ID.Width) or (WH.Y <> ID.Height));
      DC := GDICheck(CreateCompatibleDC(0));
      try
        Obj := SelectObject(DC, ID.hXOR);
        if Stretch then
        begin
          DC_XOR := GDICheck(CreateCompatibleDC(DC));
          try
            BM_XOR := GDICheck(CreateCompatibleBitmap(DC, WH.X, WH.Y));
            try
              DC_AND := GDICheck(CreateCompatibleDC(DC));
              try
                BM_AND := GDICheck(CreateMonochromeBitmap(WH.X, WH.Y));
                try
                  SetStretchBltMode(DC_XOR, STRETCH_DELETESCANS);
                  SetStretchBltMode(DC_AND, STRETCH_DELETESCANS);
                  Obj_XOR := SelectObject(DC_XOR, BM_XOR);
                  Obj_AND := SelectObject(DC_AND, BM_AND);
                  StretchBlt(DC_XOR, 0, 0, WH.X, WH.Y, DC, 0, 0, ID.Width, ID.Height, SrcCopy);
                  SelectObject(DC, ID.hAND);
                  StretchBlt(DC_AND, 0, 0, WH.X, WH.Y, DC, 0, 0, ID.Width, ID.Height, SrcCopy);
                  MaskOrBitBlt(ACanvas, P.X, P.Y, WH.X, WH.Y, DC_XOR, DC_AND, BM_XOR, BM_AND,
                    nil, 0, nil, 0, ID.Bpp, FIconDrawStyle);
                  SelectObject(DC_XOR, Obj_XOR);
                  SelectObject(DC_AND, Obj_AND);
                finally
                  DeleteObject(BM_AND);
                end;
              finally
                DeleteDC(DC_AND);
              end;
            finally
              DeleteObject(BM_XOR);
            end;
          finally
            DeleteDC(DC_XOR);
          end;
        end else
          MaskOrBitBlt(ACanvas, P.X, P.Y, ID.Width, ID.Height, DC, 0, ID.hXOR, ID.hAND,
            ID.pXOR, ID.pXORSize, ID.pAND, ID.pANDSize, ID.Bpp, FIconDrawStyle);
        SelectObject(DC, Obj);
      finally
        DeleteDC(DC);
      end;
    end;
  end;

var
  ABpp, AWidth, AHeight, I: Integer;
  P, WH, WH_S: TPoint;
begin
  with ACanvas do if FIconData <> nil then
  begin
    P := Rect.TopLeft;
    WH := Point(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
    if not FStretchEnabled then
    begin
      Inc(P.X, (WH.X - Width) div 2);
      Inc(P.Y, (WH.Y - Height) div 2);
    end;
    if FDisplayAll then
    begin
      AWidth := Width;
      AHeight := Height;
      WH_S := WH;
      for I := 0 to FIconCount - 1 do
      begin
        WH_S.X := FIconData[I].Width * WH.X div AWidth;
        WH_S.Y := FIconData[I].Height * WH.Y div AHeight;
        Display(P, WH_S, I);
        if FDisplayHorz then
          Inc(P.X, (FIconData[I].Width + FSpacing) * WH.X div AWidth)
        else
          Inc(P.Y, (FIconData[I].Height + FSpacing) * WH.Y div AHeight)
      end;
    end else
    begin
      ABpp := GetDeviceCaps(Handle, PLANES) * GetDeviceCaps(Handle, BITSPIXEL);
      if ABpp <> FBpp then
        Update;
      Display(P, WH, FCurrentIndex);
    end;
  end;
end;

function TKIcon.GetDimensions(Index: Integer): TDimension;
begin
  Result.Width := 0; Result.Height := 0;
  if (Index >= 0) and (Index < FIconCount) then
  begin
    Result.Width := FIconData[Index].Width;
    Result.Height := FIconData[Index].Height;
  end;
end;

function TKIcon.GetEmpty: Boolean;
begin
  Result := FIconData = nil;
end;

function TKIcon.GetHandles(Index: Integer): THandles;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    Result.hXOR := FIconData[Index].hXOR;
    Result.hAND := FIconData[Index].hAND;
  end else
  begin
    Result.hXOR := 0;
    Result.hAND := 0;
  end;
end;

function TKIcon.GetHeight: Integer;
begin
  if FDisplayAll and (FIconCount > 0) then
    Result := FMaxHeight
  else
    Result := Heights[FCurrentIndex];
end;

function TKIcon.GetHeights(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index].Height;
end;

function TKIcon.GetHotSpot(Index: Integer): TPoint;
begin
  Result.X := 0; Result.Y := 0;
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index].HotSpot;
end;

function TKIcon.GetIconData(Index: Integer): TIconData;
begin
  FillChar(Result, SizeOf(TIconData), #0);
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index];
end;

function TKIcon.GetWidth: Integer;
begin
  if FDisplayAll and (FIconCount > 0) then
    Result := FMaxWidth
  else
    Result := Widths[FCurrentIndex];
end;

function TKIcon.GetWidths(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < FIconCount) then
    Result := FIconData[Index].Width;
end;

procedure TKIcon.Insert(Index: Integer; const Handles: THandles);
var
  I: Integer;
begin
  if Index >= 0 then
    if Index < FIconCount then
    begin
      Inc(FIconCount);
      SetLength(FIconData, FIconCount);
      for I := FIconCount - 2 downto Index do
        FIconData[I + 1] := FIconData[I];
      FillChar(FIconData[Index], SizeOf(TIconData), 0);
      LoadHandles(Index, Handles, True);
    end else
      Add(Handles);
end;

procedure TKIcon.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  // does nothing
end;

procedure TKIcon.LoadFromHandle(Handle: HICON);
var
  Handles: THandles;
  Info: TIconInfo;
begin
  if (Handle <> 0) and GetIconInfo(Handle, Info) then
  try
    Clear;
    SetLength(FIconData, 1);
    FillChar(FIconData[0], SizeOf(TIconData), 0);
    FIconCount := 1;
    Handles.hXOR := Info.hbmColor;
    Handles.hAND := Info.hbmMask;
    LoadHandles(0, Handles, False);
  finally
    DeleteObject(Info.hbmColor);
    DeleteObject(Info.hbmMask);
  end;
end;

procedure TKIcon.LoadFromAssocFile(const FileName: string);
begin
  try
    LoadFromAssocExtension(ExtractFileExt(FileName));
  except
    LoadFromModuleByIndex(FileName, 0);
  end;
end;

procedure TKIcon.LoadFromAssocExtension(const Extension: string);
const
  IconKey = 'DefaultIcon';
var
  Code, DashPos, I: Integer;
  Module, S, T: string;
  Reg: TRegistry;
begin
  if Extension = '' then Error(SIconAssocResolveError);
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if not Reg.KeyExists(Extension) then Error(SIconAssocResolveError);
    Reg.OpenKeyReadOnly(Extension);
    try
      S := Reg.ReadString('');
    finally
      Reg.CloseKey;
    end;
    if S = '' then Error(SIconAssocResolveError);
    S := Format('%s\%s', [S, IconKey]);
    if not Reg.KeyExists(S) then Error(SIconAssocResolveError);
    Reg.OpenKeyReadOnly(S);
    try
      S := Reg.ReadString('');
      if S = '' then Error(SIconAssocResolveError);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  DashPos := Pos(',', S);
  if DashPos > 1 then
    Module := Copy(S, 1, DashPos - 1)
  else
    Module := S;
  while Module[1] in [#9, #32, '''', '"'] do System.Delete(Module, 1, 1);
  while Module[Length(Module)] in [#9, #32, '''', '"'] do System.Delete(Module, Length(Module), 1);
  if Module[1] = '%' then
  begin
    System.Delete(Module, 1, 1);
    I := Pos('%', Module);
    if (I >= 1) and
      JclSysInfo.GetEnvironmentVar(Copy(Module, 1, I - 1), T, False) then
    begin
      System.Delete(Module, 1, I);
      Module := T + Module;
    end;
  end;
  if not FileExists(Module) then Error(SIconAssocResolveError);
  T := LowerCase(ExtractFileExt(Module));
  if T = '.ico' then
    LoadFromFile(Module)
  else
  begin
    if DashPos > 0 then
    begin
      T := Copy(S, DashPos + 1, Length(S));
      while T[1] in [#9, #32] do System.Delete(T, 1, 1);
      Val(T, I, Code);
    end else
    begin
      I := 0;
      Code := 0;
    end;
    if (Code = 0) and (I >= 0) then
      LoadFromModuleByIndex(Module, I)
    else
    begin
      if Code = 0 then
        T[1] := '#';
      LoadFromModule(Module, T);
    end;
  end;
end;

procedure TKIcon.LoadFromModule(const ModuleName: string; ID: Word);
begin
  LoadFromModule(ModuleName, Format('#%d', [ID]));
end;

procedure TKIcon.LoadFromModule(const ModuleName, ResName: string);
var
  Module: HINST;
begin
  Module := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Module = 0 then Error(SIconInvalidModule);
  try
    LoadFromResource(Module, ResName);
  finally
    FreeLibrary(Module);
  end;
end;

procedure TKIcon.LoadFromModuleByIndex(const ModuleName: string; Index: Integer);
var
  Module: HINST;
begin
  Module := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Module = 0 then Error(SIconInvalidModule);
  try
    LoadFromResourceByIndex(Module, Index);
  finally
    FreeLibrary(Module);
  end;
end;

procedure TKIcon.LoadFromResource(Instance: HINST; ID: Word);
begin
  LoadFromResource(Instance, Format('#%d', [ID]));
end;

procedure TKIcon.LoadFromResource(Instance: HINST; const ResName: string);
const
  ResGroup: array[Boolean] of PAnsiChar = (RT_GROUP_ICON, RT_GROUP_CURSOR);
  ResItem: array[Boolean] of PAnsiChar = (RT_ICON, RT_CURSOR);
var
  I, IconName, ANDSize, PalSize, XORInfoSize, XORSize: Integer;
  Masked: Boolean;
  PIC: PIconCursorInRes;
  PBIn: PBitmapInfo;
  PID: PIcondata;
  BIMask: TMaskBitmapInfo;
  hGroup, hItem: HRSRC;
  hMemGroup, hMem: HGLOBAL;
  DC: HDC;
begin
  hGroup := FindResource(Instance, PAnsiChar(ResName), ResGroup[FCursor]);
  if hGroup = 0 then Error(SIconResourceError);
  hMemGroup := LoadResource(Instance, hGroup);
  if hMemGroup = 0 then Error(SIconResourceError);
  PIC := LockResource(hMemGroup);
  if (PIC.IH.idType = 1) and FCursor or (PIC.IH.idType = 2) and not FCursor then
    Error(SIconResourceError);
  DC := GetDC(0);
  try
    Clear;
    FIconCount := PIC.IH.idCount;
    SetLength(FIconData, FIconCount);
    FillChar(FIconData[0], SizeOf(TIconData) * FIconCount, 0);
    for I := 0 to PIC.IH.idCount - 1 do
    begin
      IconName := PIC.Entries[I].wEntryName;
      hItem := FindResource(Instance, PAnsiChar(IconName), ResItem[FCursor]);
      if hItem = 0 then Error(SIconResourceError);
      hMem := LoadResource(Instance, hItem);
      if hMem = 0 then Error(SIconResourceError);
      PBIn := LockResource(hMem);
      try
        PID := @FIconData[I];
        try
          if FCursor then
          begin
            PID.Width := PIC.Entries[I].Info.Cursor.Width;
            PID.Height := PIC.Entries[I].Info.Cursor.Height;
            PID.HotSpot.X := PCursorHotSpot(PBIn).xHotSpot;
            PID.HotSpot.Y := PCursorHotSpot(PBIn).yHotSpot;
            Inc(Integer(PBIn), SizeOf(TCursorHotSpot));
          end else
          begin
            PID.Width := PIC.Entries[I].Info.Icon.Width;
            PID.Height := PIC.Entries[I].Info.Icon.Height;
          end;
          PID.BytesInRes := PIC.Entries[I].dwBytesInRes;
          //PID.Bpp := PIC.Entries[I].wBitCount; // this is wrong in some icons
          PID.Bpp := PBIn.bmiHeader.biBitCount;
          CalcBitmapSizes(PID.Width, PID.Height, PID.Bpp, XORSize, ANDSize);
          PalSize := GetPaletteSize(PID.Bpp);
          XORInfoSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
          Masked := PID.BytesInRes = XORInfoSize + XORSize + ANDSize;
          if not Masked then Error(SIconFormatError);
          GetMem(PID.iXOR, XORInfoSize);
          PID.iXORSize := XORInfoSize;
          Move(PBIn^, PID.iXOR^, XORInfoSize);
          PID.iXOR.bmiHeader.biHeight := PID.iXOR.bmiHeader.biHeight div 2;
          PID.hXOR := GDICheck(CreateDIBSection(DC, PID.iXOR^,
            DIB_RGB_COLORS, PID.pXOR, 0, 0));
          if PID.pXOR <> nil then
          begin
            Move(Pointer(Integer(PBIn) + XORInfoSize)^, PID.pXOR^, XORSize);
            PID.pXORSize := XORSize;
          end else
            Error(SIconAllocationError);
          CreateMaskInfo(PID.Width, PID.Height, BIMask);
          PID.hAND := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^,
            DIB_RGB_COLORS, PID.pAND, 0, 0));
          if PID.pAND <> nil then
          begin
            Move(Pointer(Integer(PBIn) + XORInfoSize + XORSize)^, PID.pAND^, ANDSize);
            PID.pANDSize := ANDSize;
          end else
            Error(SIconAllocationError);
        except
          FreeSubimage(PID);
          raise;
        end;
      finally
        UnlockResource(hMem); // this is not necessary, but...
        FreeResource(hMem);
      end;
    end;
  finally
    ReleaseDC(0, DC);
    UnlockResource(hMemGroup); // this is not necessary, but...
    FreeResource(hMemGroup);
  end;
  Changed(Self);
end;

procedure TKIcon.LoadFromResourceByIndex(Instance: HINST; Index: Integer);
type
  PCallBack = ^TCallBack;
  TCallBack = record
    I,
    Index: Integer;
    S: string;
  end;

  function EnumIcons(hModule: HINST; lpType: DWORD; lpName: PChar; dwParam: DWORD): BOOL; stdcall;
  var
    CB: PCallBack;
  begin
    CB := PCallBack(dwParam);
    if CB.I = CB.Index then
    begin
      if HiWord(Cardinal(lpName)) = 0 then
        CB.S := Format('#%d', [Cardinal(lpName)])
      else
        CB.S := lpName;
      Result := False;
    end else
      Result := True;
    Inc(CB.I);
  end;

var
  CB: TCallBack;
begin
  CB.I := 0;
  CB.Index := Index;
  CB.S := '';
  EnumResourceNamesA(Instance, RT_GROUP_ICON, @EnumIcons, DWORD(@CB));
  if CB.S <> '' then
    LoadFromResource(Instance, CB.S)
  else if CB.I = 0 then
    Error(SIconInvalidModule)
  else
    Error(SIconIndexError);
end;

procedure TKIcon.LoadFromResourceStream(Stream: TStream);
var
  I, Offset, BkPos, Size: Integer;
  IsCursor: Boolean;
  P, Q: Pointer;
  IH: TIconHeader;
  AI: array of TIconCursorDirEntry;
  PII: PIconCursorDirEntry;
  IR: TIconCursorDirEntryInRes;
  MS: TMemoryStream;
begin
  P := nil;
  MS := TMemoryStream.Create;
  try
    Stream.Read(IH, SizeOf(TIconHeader));
    IsCursor := IH.idType = 2;
    MS.Write(IH, SizeOf(TIconHeader));
    Offset := SizeOf(TIconHeader) + IH.idCount * SizeOf(TIconCursorDirEntry);
    SetLength(AI, IH.idCount);
    if IsCursor then
    begin
      for I := 0 to IH.idCount - 1 do
      begin
        PII := @AI[I];
        Stream.Read(IR, SizeOf(TIconCursorDirEntryInRes));
        PII.Width := IR.Info.Cursor.Width;
        PII.Height := IR.Info.Cursor.Height;
        PII.ColorCount := 0;
        PII.dwBytesInRes := IR.dwBytesInRes - SizeOf(TCursorHotSpot);
        PII.dwImageOffset := Offset;
        Inc(Offset, PII.dwBytesInRes);
      end;
      BKPos := Stream.Position;
      for I := 0 to IH.idCount - 1 do
      begin
        PII := @AI[I];
        Size := PII.dwBytesInRes + SizeOf(TCursorHotSpot);
        ReallocMem(P, Size);
        Stream.Read(P^, Size);
        PII.Info.wX := PCursorHotSpot(P).xHotSpot;
        PII.Info.wY := PCursorHotSpot(P).yHotSpot;
        MS.Write(PII^, SizeOf(TIconCursorDirEntry));
      end;
      Stream.Position := BKPos;
      for I := 0 to IH.idCount - 1 do
      begin
        PII := @AI[I];
        Size := PII.dwBytesInRes + SizeOf(TCursorHotSpot);
        ReallocMem(P, Size);
        Stream.Read(P^, Size);
        Q := P;
        Inc(Integer(Q), SizeOf(TCursorHotSpot));
        MS.Write(Q^, PII.dwBytesInRes);
      end;
    end else
    begin
      for I := 0 to IH.idCount - 1 do
      begin
        PII := @AI[I];
        Stream.Read(IR, SizeOf(TIconCursorDirEntryInRes));
        PII.Width := IR.Info.Icon.Width;
        PII.Height := IR.Info.Icon.Height;
        PII.ColorCount := IR.Info.Icon.ColorCount;
        PII.Info.wPlanes := IR.wPlanes;
        PII.Info.wBitCount := IR.wBitCount;
        PII.dwBytesInRes := IR.dwBytesInRes;
        PII.dwImageOffset := Offset;
        MS.Write(PII^, SizeOf(TIconCursorDirEntry));
        Inc(Offset, PII.dwBytesInRes);
      end;
      for I := 0 to IH.idCount - 1 do
      begin
        PII := @AI[I];
        ReallocMem(P, PII.dwBytesInRes);
        Stream.Read(P^, PII.dwBytesInRes);
        MS.Write(P^, PII.dwBytesInRes);
      end;
    end;
    MS.Position := 0;
    LoadFromStream(MS);
  finally
    MS.Free;
    FreeMem(P);
  end;
end;

procedure TKIcon.LoadFromStream(Stream: TStream);
var
  I, ANDSize, PalSize, XORInfoSize, XORSize: Integer;
  Masked: Boolean;
  PID: PIconData;
  IH: TIconHeader;
  II: TIconCursorDirEntry;
  BI: TBitmapInfoHeader;
  BIMask: TMaskBitmapInfo;
  DC: HDC;
begin
  if Stream <> nil then
  begin
    DC := GetDC(0);
    try
      Clear;
      Stream.Read(IH, SizeOf(TIconHeader));
      FCursor := IH.idType = 2;
      FIconCount := IH.idCount;
      SetLength(FIconData, FIconCount);
      FillChar(FIconData[0], SizeOf(TIconData) * FIconCount, 0);
      for I := 0 to FIconCount - 1 do
      begin
        PID := @FIconData[I];
        Stream.Read(II, SizeOf(TIconCursorDirEntry));
        if FCursor then
        begin
          PID.HotSpot.X := II.Info.wX;
          PID.HotSpot.Y := II.Info.wY;
        end else
        begin
          PID.Width := II.Width;
          PID.Height := II.Height;
        end;
        PID.BytesInRes := II.dwBytesInRes;
        //PID.Bpp := II.wBitCount; // this is wrong in some icons
      end;
      for I := 0 to FIconCount - 1 do
      begin
        PID := @FIconData[I];
        try
          Stream.Read(BI, SizeOf(TBitmapInfoHeader));
          PID.Bpp := BI.biBitCount;
          if FCursor then
          begin
            PID.Width := BI.biWidth;
            PID.Height := BI.biHeight shr 1;
          end;
          PalSize := GetPaletteSize(PID.Bpp);
          CalcBitmapSizes(PID.Width, PID.Height, PID.Bpp, XORSize, ANDSize);
          XORInfoSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
          Masked := PID.BytesInRes = XORInfoSize + XORSize + ANDSize;
          if not Masked then Error(SIconFormatError);
          BI.biHeight := BI.biHeight div 2;
          GetMem(PID.iXOR, XORInfoSize);
          PID.iXORSize := XORInfoSize;
          PID.iXOR.bmiHeader := BI;
          PID.iXOR.bmiHeader.biSizeImage := 0;
          Stream.Read(PID.iXOR.bmiColors, PalSize * SizeOf(TRGBQuad));
          PID.hXOR := GDICheck(CreateDIBSection(DC, PID.iXOR^,
            DIB_RGB_COLORS, PID.pXOR, 0, 0));
          if PID.pXOR <> nil then
          begin
            Stream.Read(PID.pXOR^, XORSize);
            PID.pXORSize := XORSize;
          end else
            Error(SIconAllocationError);
          CreateMaskInfo(PID.Width, PID.Height, BIMask);
          PID.hAND := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^,
            DIB_RGB_COLORS, PID.pAND, 0, 0));
          if PID.pAND <> nil then
          begin
            Stream.Read(PID.pAND^, ANDSize);
            PID.pANDSize := ANDSize;
          end else
            Error(SIconAllocationError);
        except
          FreeSubimage(PID);
          raise;
        end;
      end;
    finally
      ReleaseDC(0, DC);
    end;
    Changed(Self);
  end;
end;

procedure TKIcon.LoadHandles(Index: Integer; const Handles: THandles; OrigBpp: Boolean);
var
  ANDSize, PalSize, XORSize, XORInfoSize: Integer;
  PID: PIconData;
  BInfo: Windows.TBitmap;
  BIMask: TMaskBitmapInfo;
  P: Pointer;
  DC: HDC;
  hBmp: HBITMAP;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    PID := @FIconData[Index];
    if (Handles.hAND = 0) or
      (Handles.hXOR = PID.hXOR) or (Handles.hAND = PID.hXOR) or
      (Handles.hXOR = PID.hAND) or (Handles.hAND = PID.hAND) then
      Error(SIconBitmapError);
    FreeSubimage(PID);
    DC := GetDC(0);
    try
      try
        if Handles.hXOR <> 0 then
        begin
          GetObject(Handles.hXOR, SizeOf(Windows.TBitmap), @BInfo);
          PID.Height := BInfo.bmHeight;
          if OrigBpp or (FInHandleBpp = 0) then
            PID.Bpp := BInfo.bmPlanes * BInfo.bmBitsPixel
          else
            PID.Bpp := FInHandleBpp;
        end else
        begin // must be a monochrome icon - not fully tested
          GetObject(Handles.hAND, SizeOf(Windows.TBitmap), @BInfo);
          PID.Height := BInfo.bmHeight div 2;
          PID.Bpp := 1;
        end;
        PID.Width := BInfo.bmWidth;
        CalcBitmapSizes(PID.Width, PID.Height, PID.Bpp, XORSize, ANDSize);
        PalSize := GetPaletteSize(PID.Bpp);
        XORInfoSize := SizeOf(TBitmapInfoHeader) + PalSize * SizeOf(TRGBQuad);
        GetMem(PID.iXOR, XORInfoSize);
        PID.iXORSize := XORInfoSize;
        FillChar(PID.iXOR^, XORInfoSize, 0);
        PID.BytesInRes := XORInfoSize;
        PID.iXOR.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
        PID.iXOR.bmiHeader.biWidth := PID.Width;
        PID.iXOR.bmiHeader.biHeight := PID.Height;
        PID.iXOR.bmiHeader.biPlanes := 1;
        PID.iXOR.bmiHeader.biBitCount := PID.Bpp;
        PID.iXOR.bmiHeader.biCompression := BI_RGB;
        if Handles.hXOR <> 0 then hBmp := Handles.hXOR else hBmp := Handles.hAND;
        // the following method of getting of a hBmp color table is a M$ holy shit!!!
        // (as the entire bitmap handling)
        GetDIBits(DC, hBmp, 0, PID.Height, nil, PID.iXOR^, DIB_RGB_COLORS);
        PID.hXOR := GDICheck(CreateDIBSection(DC, PID.iXOR^,
          DIB_RGB_COLORS, PID.pXOR, 0, 0));
        if PID.pXOR <> nil then
        begin
          GetDIBits(DC, hBmp, 0, PID.Height, PID.pXOR,
            PID.iXOR^, DIB_RGB_COLORS);
          PID.pXORSize := XORSize;
          if (PID.Bpp = 32) and FInHandleFullAlpha then
            FillAlphaIfNone(PColor32Array(PID.pXOR),
              XORSize, $FF);
          Inc(PID.BytesInRes, XORSize);
        end else
          Error(SIconAllocationError);
        CreateMaskInfo(PID.Width, PID.Height, BIMask);
        PID.hAND := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^,
          DIB_RGB_COLORS, PID.pAND, 0, 0));
        if PID.pAND <> nil then
        begin
          if Handles.hXOR <> 0 then
          begin
            GetDIBits(DC, Handles.hAND, 0, PID.Height, PID.pAND,
              PBitmapInfo(@BIMask)^, DIB_RGB_COLORS);
          end else
          begin
            GetMem(P, ANDSize * 2);
            try
              BIMask.Header.biHeight := 2 * PID.Height;
              GetDIBits(DC, Handles.hAND, 0, PID.Height * 2, P,
                PBitmapInfo(@BIMask)^, DIB_RGB_COLORS);
              Move(P^, PID.pAND^, ANDSize);
            finally
              FreeMem(P);
            end;
          end;
          PID.pANDSize := ANDSize;
          Inc(PID.BytesInRes, ANDSize);
        end else
          Error(SIconAllocationError);
      except
        FreeSubimage(PID);
        raise;
      end;
    finally
      ReleaseDC(0, DC);
    end;
    Changed(Self);
  end;
end;

procedure TKIcon.MaskFromColor(Index: Integer; Color: TColor; HasAlpha: Boolean = False);
var
  PID: PIconData;
  DC: HDC;
  OldObj: HGDIObj;
  BM: TJclBitmap32;
  R: TRect;
  ByteMask: Byte;
  I, J, K, L, LAnd: Integer;
  ColorMask: Cardinal;
  P: PColor32Array;
  Q: PByteArray;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    Color := K_RGBToBGR(Color);
    PID := @FIconData[Index];
    DC := 0;
    BM := TJclBitmap32.Create;
    try
      DC := GDICheck(CreateCompatibleDC(0));
      OldObj := SelectObject(DC, PID.hXOR);
      BM.SetSize(PID.Width, PID.Height);
      R := Rect(0, 0, PID.Width, PID.Height);
      BM.Draw(R, R, DC);
      P := BM.Bits;
      FillChar(PID.pAND^, PID.pANDSize, $FF);
      LAnd := CalcByteWidth(PID.Width, 1);
      Q := PID.pAND;
      Inc(Cardinal(Q), PID.pANDSize - LAnd);
      K := 0;
      if HasAlpha then ColorMask := $FFFFFFFF else ColorMask := $00FFFFFF;
      for I := 0 to PID.Height - 1 do
      begin
        ByteMask := $7F;
        for J := 0 to PID.Width - 1 do
        begin
          L := J shr 3;
          if P[K] and Colormask <> Cardinal(Color) then
            Q[L] := Q[L] and ByteMask;
          asm
            ror ByteMask, 1
          end;
          Inc(K);
        end;
        Dec(Cardinal(Q), LAnd);
      end;
      SelectObject(DC, OldObj);
    finally
      if DC <> 0 then DeleteDC(DC);
      BM.Free;
    end;
    Changed(Self);
  end;
end;

procedure TKIcon.SaveToStream(Stream: TStream);
var
  I, Offset, RSize: Integer;
  IH: TIconHeader;
  PID: PIconData;
  II: TIconCursorDirEntry;
begin
  if (Stream <> nil) and (FIconData <> nil) then
  begin
    Offset := SizeOf(TIconHeader) + FIconCount * SizeOf(TIconCursorDirEntry);
    IH.idReserved := 0;
    if FCursor then IH.idType := 2 else IH.idType := 1;
    IH.idCount := 0;
    for I := 0 to FIconCount - 1 do
      if FIconData[I].iXOR <> nil then
        Inc(IH.idCount);
    Stream.Write(IH, SizeOf(TIconHeader));
    FillChar(II, SizeOf(TIconCursorDirEntry), 0);
    for I := 0 to FIconCount - 1 do
    begin
      PID := @FIconData[I];
      if PID.iXOR <> nil then
      begin
        II.Width := PID.Width;
        II.Height := PID.Height;
        II.ColorCount := GetPaletteSize(PID.Bpp);
        if FCursor then
        begin
          II.Info.wX := PID.HotSpot.X;
          II.Info.wY := PID.HotSpot.Y;
        end else
        begin
          II.Info.wPlanes := 1;
          II.Info.wBitCount := PID.Bpp;
        end;
        RSize := PID.iXORSize + PID.pXORSize + PID.pANDSize;
        II.dwBytesInRes := RSize;
        II.dwImageOffset := Offset;
        Stream.Write(II, SizeOf(TIconCursorDirEntry));
        Inc(Offset, RSize);
      end;
    end;
    for I := 0 to FIconCount - 1 do
    begin
      PID := @FIconData[I];
      if PID.iXOR <> nil then
      begin
        PID.iXOR.bmiHeader.biHeight := PID.iXOR.bmiHeader.biHeight * 2;
        Stream.Write(PID.iXOR^, PID.iXORSize);
        PID.iXOR.bmiHeader.biHeight := PID.iXOR.bmiHeader.biHeight div 2;
        Stream.Write(PID.pXOR^, PID.pXORSize);
        Stream.Write(PID.pAND^, PID.pANDSize);
      end;
    end;
  end;
end;

procedure TKIcon.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
begin
  // does nothing
end;

procedure TKIcon.SetCurrentIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < FIconCount) and (Value <> FCurrentIndex) then
  begin
    FCurrentIndex := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetDisplayAll(Value: Boolean);
begin
  if Value <> FDisplayAll then
  begin
    FDisplayAll := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetDisplayHorz(Value: Boolean);
begin
  if Value <> FDisplayHorz then
  begin
    FDisplayHorz := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetDimensions(Index: Integer; Value: TDimension);
begin
  if (Index >= 0) and (Index < FIconCount) and
    (Value.Width > 0) and (Value.Height > 0) and
    (Value.Width <> Widths[Index]) and (Value.Width <> Heights[Index]) then
  begin
    UpdateDim(Index, Value);
    Changed(Self);
  end;
end;

procedure TKIcon.SetHandles(Index: Integer; Value: THandles);
begin
  LoadHandles(Index, Value, True);
end;

procedure TKIcon.SetHeight(Value: Integer);
begin
  if not FDisplayAll then
    Heights[FCurrentIndex] := Value;
end;

procedure TKIcon.SetHeights(Index: Integer; Value: Integer);
var
  D: TDimension;
begin
  D.Width := Widths[Index];
  D.Height := Value;
  Dimensions[Index] := D;
end;

procedure TKIcon.SetHotSpot(Index: Integer; Value: TPoint);
var
  PID: PIconData;
begin
  if (Index >= 0) and (Index < FIconCount) then
  begin
    PID := @FIconData[Index];
    if (PID.HotSpot.X <> Value.X) or (PID.HotSpot.Y <> Value.Y) then
    begin
      PID.HotSpot := Value;
      Changed(Self);
    end;
  end;
end;

procedure TKIcon.SetIconDrawStyle(Value: TIconDrawStyle);
begin
  if Value <> FIconDrawStyle then
  begin
    FIconDrawStyle := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetInHandleBpp(Value: Integer);
begin
  if Value in [0, 1, 4, 8, 32] then
    FInHandleBpp := Value;
end;

procedure TKIcon.SetOptimalIcon(Value: Boolean);
begin
  if Value <> FOptimalIcon then
  begin
    FOptimalIcon := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetOverSizeWeight(Value: Single);
begin
  if Value <> FOverSizeWeight then
  begin
    FOverSizeWeight := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetRequestedSize(Value: TDimension);
begin
  if (Value.Width > 0) and (Value.Height > 0) then
  begin
    FRequestedSize := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetStretchEnabled(Value: Boolean);
begin
  if Value <> FStretchEnabled then
  begin
    FStretchEnabled := Value;
    Changed(Self);
  end;
end;

procedure TKIcon.SetTransparent(Value: Boolean);
begin
  if FCreating then
    inherited
  else
    // Ignore assignments to this property.
    // Icons are always transparent.
end;

procedure TKIcon.SetWidth(Value: Integer);
begin
  if not FDisplayAll then
    Widths[FCurrentIndex] := Value;
end;

procedure TKIcon.SetWidths(Index: Integer; Value: Integer);
var
  D: TDimension;
begin
  D.Width := Value;
  D.Height := Heights[Index];
  Dimensions[Index] := D;
end;

procedure TKIcon.Update;
var
  dW, dH, BestBpp, I, MaxWeight, Weight: Integer;
  DC: HDC;
  PID: PIconData;
begin
  FBpp := 0;
  FMaxWidth := 0;
  FMaxHeight := 0;
  if FIconData <> nil then
  begin
    DC := GetDC(0);
    try
      FBpp := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      MaxWeight := MaxInt;
      for I := 0 to FIconCount - 1 do
      begin
        PID := @FIconData[I];
        if FDisplayAll and FDisplayHorz then
        begin
          Inc(FMaxWidth, PID.Width);
          if I <> 0 then Inc(FMaxWidth, FSpacing);
        end else
          if PID.Width > FMaxWidth then FMaxWidth := PID.Width;
        if FDisplayAll and not FDisplayHorz then
        begin
          Inc(FMaxHeight, PID.Height);
          if I <> 0 then Inc(FMaxHeight, FSpacing);
        end else
          if PID.Height > FMaxHeight then FMaxHeight := PID.Height;
      end;
      if FOptimalIcon and (FIconCount >= 2) then
      begin
        FCurrentIndex := 0;
        BestBpp := FIconData[0].Bpp;
        for I := 0 to FIconCount - 1 do
        begin
          PID := @FIconData[I];
          if (PID.Bpp <= FBpp) and (PID.Bpp >= BestBpp) then
          begin
            BestBpp := PID.Bpp;
            dW := FRequestedSize.Width - PID.Width;
            dH := FRequestedSize.Height - PID.Height;
            if dW < 0 then DW := Round(-DW * FOverSizeWeight);
            if dH < 0 then dH := Round(-DH * FOverSizeWeight);
            Weight := dW + dH;
            if Weight <= MaxWeight then
            begin
              MaxWeight := Weight;
              FCurrentIndex := I;
            end;
          end;
        end;
      end  
      else if (FCurrentIndex < 0) or (FCurrentIndex >= FIconCount) then
        FCurrentIndex := 0;
    finally
      ReleaseDC(0, DC);
    end;
  end else
    FCurrentIndex := -1;
end;

procedure TKIcon.UpdateDim(Index: Integer; Value: TDimension);

  procedure BitMove(const Src, Dest; BitSize, BitOffset: Integer);
  asm
    // eax: Src
    // ecx: BitSize
    // edx: Dest
    // stack: BitOffset
    // push registers that must be preserved
    push esi
    push edi
    push ebx
    // set registers for register adressing
    mov esi, eax
    mov edi, edx
    // test for scroll direction
    mov edx, BitOffset
    cmp edx, 0
    js @left
    // perform move
    mov ebx, edx
    shr ebx, 3
    add edi, ebx
    and edx, $07
    jnz @bitwise_right
    // bytewise move
    mov edx, ecx
    shr ecx, 3
    rep movsb
    and dl, $07
    jz @exit
    mov cl, dl
    mov al, [esi]
    rol eax, cl
    mov al, [edi]
    ror eax, cl
    mov [edi], al
    jmp @exit
  @bitwise_right:
    // bitwise move
    mov ebx, ecx
    mov cl, dl
    xor ch, ch
    mov dl, $7F
    ror dl, cl
    mov dh, dl
    not dh
  @R00:
    mov ah, [esi]
    ror ah, cl
    and ah, dh
    mov al, [edi]
    and al, dl
    or al, ah
    mov [edi], al
    dec ebx
    jz @exit
    inc ch
    and ch, $07
    jnz @R01
    inc esi
  @R01:
    ror dl, 1
    ror dh, 1
    test dh, $80
    jz @R00
    inc edi
    jmp @R00
  @left:
    // perform scroll
    neg edx
    mov ebx, edx
    shr ebx, 3
    add esi, ebx
    and edx, $07
    jnz @bitwise_left
    // bytewise move
    mov edx, ecx
    shr ecx, 3
    rep movsb
    and dl, $07
    jz @exit
    mov cl, dl
    mov al, [esi]
    rol eax, cl
    mov al, [edi]
    ror eax, cl
    mov [edi], al
    jmp @exit
  @bitwise_left:
    // bitwise move
    mov ebx, ecx
    mov cl, dl
    mov ch, cl
    mov dl, $7F
    mov dh, dl
    not dh
  @L00:
    mov ah, [esi]
    rol ah, cl
    and ah, dh
    mov al, [edi]
    and al, dl
    or al, ah
    mov [edi], al
    dec ebx
    jz @exit
    inc ch
    and ch, $07
    jnz @L01
    inc esi
  @L01:
    ror dl, 1
    ror dh, 1
    test dh, $80
    jz @L00
    inc edi
    jmp @L00
  @exit:
    // pop the preserved registers
    pop ebx
    pop edi
    pop esi
  end;

var
  BitOffset, J, Size, XOR1, XOR2, AND1, AND2,
  X, Y, HOffset, VOffset: Integer;
  PID: PIconData;
  PBI: PBitmapInfoHeader;
  BIMask: TMaskBitmapInfo;
  P: PByteArray;
  hBmp: HBITMAP;
  DC: HDC;
begin
  PID := @FIconData[Index];
  if PID.iXOR <> nil then
  begin
    PBI := PBitmapInfoHeader(PID.iXOR);
    P := nil;
    DC := GetDC(0);
    try
      try
        CalcByteWidths(PID.Width, PID.Bpp, XOR1, AND1);
        CalcByteWidths(Value.Width, PID.Bpp, XOR2, AND2);
        PBI.biWidth := Value.Width;
        PBI.biHeight := Value.Height;
        PBI.biSizeImage := XOR2 * Value.Height;
        if FAlignStyle = asCenter then
        begin
          HOffset := (Value.Width - PID.Width) div 2;
          VOffset := (Value.Height - PID.Height) div 2;
        end else
        begin
          HOffset := 0;
          VOffset := 0;
        end;
        Y := Min(PID.Height, Value.Height);
        BitOffset := HOffset * PID.Bpp;
        hBmp := GDICheck(CreateDIBSection(DC, PBitmapInfo(PBI)^, DIB_RGB_COLORS, Pointer(P), 0, 0));
        if P = nil then Error(SIconAllocationError);
        X := Min(PID.Width, Value.Width) * PID.Bpp;
        Size := XOR2 * Value.Height;
        FillChar(P^, Size, #0);
        for J := 1 to Y do
        begin
          if VOffset >= 0 then
            BitMove(PByteArray(PID.pXOR)[(PID.Height - J) * XOR1],
              P[(Value.Height - J - VOffset) * XOR2], X, BitOffset)
          else
            BitMove(PByteArray(PID.pXOR)[(PID.Height - J + VOffset) * XOR1],
              P[(Value.Height - J) * XOR2], X, BitOffset);
        end;
        DeleteObject(PID.hXOR);
        PID.pXOR := P;
        PID.pXORSize := Size;
        PID.hXOR := hBmp;
        CreateMaskInfo(PID.Width, PID.Height, BIMask);
        hBmp := GDICheck(CreateDIBSection(DC, PBitmapInfo(@BIMask)^, DIB_RGB_COLORS, Pointer(P), 0, 0));
        if P = nil then Error(SIconAllocationError);
        X := Min(PID.Width, Value.Width);
        Size := AND2 * Value.Height;
        FillChar(P^, Size, #$FF);
        for J := 1 to Y do
        begin
          if VOffset >= 0 then
            BitMove(PByteArray(PID.pAND)[(PID.Height - J) * AND1],
              P[(Value.Height - J - VOffset) * AND2], X, HOffset)
          else
            BitMove(PByteArray(PID.pAND)[(PID.Height - J + VOffset) * AND1],
              P[(Value.Height - J) * AND2], X, HOffset);
        end;
        DeleteObject(PID.hAND);
        PID.pAND := P;
        PID.pANDSize := Size;
        PID.hAND := hBmp;
        PID.Width := Value.Width;
        PID.Height := Value.Height;
      except
        FreeSubimage(PID);
        Error(SIconResizingError);
      end;
    finally
      ReleaseDC(0, DC);
    end;
  end;
end;

initialization
  TPicture.UnregisterGraphicClass(Graphics.TIcon);
  TPicture.RegisterFileFormat('ico', SVIcons, KIcon.TIcon);
  TPicture.RegisterFileFormat('cur', SVCursors, KIcon.TIcon);
finalization
  //not necessary, but...
  TPicture.UnregisterGraphicClass(KIcon.TIcon);
  TPicture.RegisterFileFormat('ico', SVIcons, Graphics.TIcon);
end.
