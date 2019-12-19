unit guitextobject;

//renders a font.
//right now it just renders a single texture with the required text and updates it as needed
//todo: build a fontmap

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, guiobject, gamepanel, gl,glext,math, controls;

type
  TTextAlign=(taLeft, taCenter, taRight);

  TGUITextObject=class(TGuiObject)
  private
    ftext: string; //just as an indication if the texture needs to be updated
    fbackgroundcolor: tcolor; //for anti alliasing
    fforegroundcolor: tcolor;
    ftexture: integer;

    ffont: tfont;

    fwidth, fheight: single;
    fLineSpacing: integer;
    fbackgroundAlpha: byte;
    fKeepAspectRatio: boolean;
    procedure setFont(f: tfont);
  protected
    function getTexture: integer; override;
    function getWidth:single; override;
    procedure setWidth(w: single); override;
    function getHeight:single; override;
    procedure setHeight(h:single); override;
  public
    minWidth: integer; //minimal pixel width for the texture
    minheight: integer;
    firstTextBecomesMinWidth: boolean;
    textAlignment: TTextAlign;
    procedure setText(text: string);
    constructor create(owner: TGamePanel=nil);
    destructor destroy; override;
    property Text: string read ftext write setText;
    property font: tfont read ffont write setFont;
    property linespacing: integer read flinespacing write flinespacing;
    property color: tcolor read fforegroundcolor write fforegroundcolor;
    property bcolor: tcolor read fbackgroundcolor write fbackgroundcolor;
    property backgroundAlpha: byte read fbackgroundAlpha write fbackgroundAlpha default 0;
    property keepAspectRatio: boolean read fKeepAspectRatio write fkeepAspectRatio;
  end;

implementation

function TGUITextObject.getTexture: integer;
begin
  result:=ftexture;
end;

function TGUITextObject.getWidth:single;
begin
  result:=fwidth;
end;

procedure TGUITextObject.setWidth(w: single);
begin
  fwidth:=w;
end;

function TGUITextObject.getHeight:single;
begin
  result:=fheight;
end;

procedure TGUITextObject.setHeight(h: single);
begin
  fheight:=h;
end;


procedure TGUITextObject.setText(text: string);
var
  img: TBitmap;
  pp: pointer;
  p: pdword;

  th, tw,iw,ih: integer;
  i: integer;
  backgroundcolorbgr: dword;

  sl: tstringlist;

  ba: dword;

  ar: single;
begin
  if ftext<>text then
  begin
    sl:=TStringList.create;
    sl.text:=text;

    //update the texture
    img:=TBitmap.Create;

    img.PixelFormat:=pf32bit;
    img.canvas.font:=font;
    tw:=img.Canvas.TextWidth(text);


    tw:=0;
    th:=0;
    for i:=0 to sl.Count-1 do
    begin
      if i>0 then inc(th, fLineSpacing);
      inc(th,img.Canvas.TextHeight(sl[i]));

      tw:=max(img.Canvas.TextWidth(sl[i]), tw);
    end;

    if (minwidth=0) and firstTextBecomesMinWidth then
      minwidth:=tw;

    if (minheight=0) and firstTextBecomesMinWidth then
      minheight:=th;


    if fKeepAspectRatio then
    begin
      tw:=max(minWidth,tw);
      th:=max(minHeight,th);

      //increase tw and th to the aspect ratio given
      ar:=height/width;
      i:=trunc(tw*ar);
      th:=max(i,th);

      ar:=width/height;
      i:=trunc(th*ar);
      tw:=max(i,tw);

      img.width:=max(minWidth,tw);
      img.Height:=max(minHeight,th);

    end
    else
    begin
      img.width:=max(minWidth,tw);
      img.Height:=max(minHeight,th);
    end;
    img.canvas.font.Color:=fforegroundcolor;
    img.canvas.Brush.Color:=fbackgroundcolor; //will be made transparant
    img.Canvas.FillRect(0,0,img.width, img.height);

    backgroundcolorbgr:=ColorToRGB(fbackgroundcolor);

    backgroundcolorbgr:=((backgroundcolorbgr shr 16) and $ff)+((backgroundcolorbgr shl 16) and $ff0000)+(backgroundcolorbgr and $ff00);

   // backgroundcolorbgr:=Swap(backgroundcolorbgr);

    th:=0;
    for i:=0 to sl.count-1 do
    begin
      tw:=img.Canvas.TextWidth(sl[i]);
      case textAlignment of
        taLeft:   img.Canvas.TextOut(0,th,sl[i]);
        taCenter: img.Canvas.TextOut((img.width div 2)-(tw div 2),th,sl[i]);
        taRight:  img.Canvas.TextOut(img.width-tw,th,sl[i]);
      end;
      inc(th,img.Canvas.TextHeight(sl[i])+fLineSpacing);
    end;

    iw:=img.width;
    ih:=img.height;

    glBindTexture(GL_TEXTURE_2D, ftexture);
    glActiveTexture(GL_TEXTURE0);

    pp:=img.RawImage.Data;


    //FPC sets the alpha channel to 100% transparant by default

    p:=pp;
    i:=0;

    ba:=backgroundAlpha shl 24;
    while i<img.RawImage.DataSize do
    begin
      if p^=backgroundcolorbgr then
        p^:=p^ or ba //0
      else
        p^:=p^ or $ff000000;

      inc(i,4);
      inc(p);
    end;


    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, iw, ih, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    img.free;
  end;
end;

procedure TGUITextObject.setFont(f: tfont);
begin
  ffont.Assign(f);
end;

destructor TGUITextObject.destroy;
begin
  glDeleteTextures(1,@ftexture);
  inherited destroy;
end;

constructor TGUITextObject.create(owner: TGamePanel=nil);
begin
  inherited create(owner);

  textAlignment:=taCenter;

  rotationpoint.x:=-1; //top left
  rotationpoint.y:=-1;
  glGenTextures(1, @ftexture);

  fbackgroundcolor:=clRed;
  fforegroundcolor:=clBlue;

  ffont:=TFont.Create;
  ffont.Quality:=fqNonAntialiased;

  fkeepAspectRatio:=true;

  backgroundAlpha:=0;
end;

end.

