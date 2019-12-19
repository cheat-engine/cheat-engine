unit staticguiobject;

//just shows a single image, non updating

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gamepanel, guiobject, gl, glext, globals, Graphics, controls, types;

type
  TStaticGUIObject=class(TGUIObject)
  private
    fheight, fwidth: single;
    ftexture: integer;
    img: TPortableNetworkGraphic;
  protected
    function getTexture: integer; override;
    function getWidth:single; override;
    function getHeight:single; override;
    function mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean; override;
  public
    constructor create(owner: TGamePanel; image: string; h,w: single);
    destructor destroy; override;
  end;


implementation

function TStaticGUIObject.mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean;
var gamepos: TPointF;

  objectpos: TPointF;
  objectx: single;
  objecty: single;
  pp: pdword;

  ppos: tpoint;
  iw,ih: integer;
begin
  //convert the x and y coords to screen coords
  result:=false;

  if meventtype=0 then
  begin
    //I could use the default gui mhandler, but this lets me be more picky about the transparent spots
    objectPos:=getTopLeftCorner;

    gamepos:=TGamePanel(sender).PixelPosToGamePos(mx,my);

    //convert x,y pos to actual x,y pos based on rotationpoint (currenty no rotation support for gui objects)
    objectpos:=getTopLeftCorner;
    objectx:=objectpos.x; //x-(width/2)*(rotationpoint.x+1);
    objecty:=objectpos.y; //y-(height/2)*(rotationpoint.y+1);

    if (gamepos.x>=objectx) and (gamepos.x<objectx+width) and (gamepos.y>=objecty) and (gamepos.y<objecty+height) then
    begin
      //now check what pixel is clicked
     // glBindTexture(GL_TEXTURE_2D, getTexture);
     // glGetTexImage(GL_TEXTURE_2D, 0, GL_BGRA,  GL_UNSIGNED_BYTE,@pp);

      iw:=img.width;
      ih:=img.height;

      ppos:=TGamePanel(sender).GamePosToPixelPos(-1+(gamepos.x-objectx)*2, -1+(gamepos.y-objecty)*2);

      pp:=pdword(img.RawImage.Data);

      if (pp[iw*ppos.y+ppos.x] shr 24)>127 then
      begin
        if assigned(fOnClick) then
          exit(fOnClick(self));
      end;
    end;
  end;
end;

function TStaticGUIObject.getWidth:single;
begin
  result:=fwidth
end;

function TStaticGUIObject.getHeight:single;
begin
  result:=fheight;
end;

function TStaticGUIObject.getTexture: integer;
begin
  result:=ftexture;
end;


constructor TStaticGUIObject.create(owner: TGamePanel; image: string; h,w: single);
var
  pp: pointer;
  //p: TPortableNetworkGraphic;
begin
  inherited create(owner);


  fheight:=h;
  fwidth:=w;
  rotationpoint.x:=-1; //top left
  rotationpoint.y:=-1;

  glGenTextures(1, @ftexture);
  img:=TPortableNetworkGraphic.Create;
  img.LoadFromFile(assetsfolder+image);


  glBindTexture(GL_TEXTURE_2D, ftexture);
  glActiveTexture(GL_TEXTURE0);

  pp:=img.RawImage.Data;

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.Width, img.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);


end;

destructor TStaticGUIObject.destroy;
begin
  glDeleteTextures(1,@ftexture);
  img.free;
  inherited destroy;
end;

end.

