unit platformplayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,glext,glu, globals, gameobject, renderobject, graphics,
  gameobjectwithhealth, healthbar;

type
  TPlatformPlayer=class(TGameObject)
  private
    instances: integer;
    ftexture: integer; static;
    fwidth: single;
    fheight: single;
    //animationpos: integer;
  protected
    function getWidth:single; override;
    function getHeight:single; override;
    function getTexture: integer; override;
  public
    constructor create;
    destructor destroy; override;
  end;


implementation

function TPlatformPlayer.getWidth:single;
begin
  result:=0.06;
end;

function TPlatformPlayer.getHeight:single;
begin
  result:=0.15;
end;

function TPlatformPlayer.getTexture: integer;
begin
  result:=ftexture; //todo: animate
end;



constructor TPlatformPlayer.create;
var img: tpicture;
  pp: pointer;
begin
  inherited create;

  CollisionType:=ctUnrotatedRectangles;

  glGenTextures(1, @ftexture);

  img:=tpicture.Create;
  img.LoadFromFile(assetsfolder+'platformplayer.png');


  glBindTexture(GL_TEXTURE_2D, ftexture);
  glActiveTexture(GL_TEXTURE0);

  //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 2, 2, 0, GL_RGB, GL_FLOAT, @pixels[0]);
  pp:=img.BITMAP.RawImage.Data;

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.Width, img.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  img.free;
end;

destructor TPlatformPlayer.destroy;
begin
  dec(instances);
  if instances=0 then //destroy the texture
    glDeleteTextures(1,@ftexture);

  inherited destroy;
end;


end.

