unit platformenemy;

{$mode objfpc}{$H+}

//target practive object

interface

uses
  Classes, SysUtils, gl,glext,glu, gameobject, renderobject, graphics, healthbar,
  globals;

type
  TPlatformEnemy=class(TGameObject)  //tutorial step 1. Targets have their own health (so not a TGameObjectWithHealth)
  private
    instances: integer; static;
    ftexture: integer; static;
    fwidth: single; static;
    fheight: single; static;
  protected
    function getWidth:single; override;
    function getHeight:single; override;
    function getTexture: integer; override;
  public
    minx,maxx: single;
    speed: single;
    procedure travel(time:single);
    constructor create;
    destructor destroy; override;
  end;


implementation

procedure TPlatformEnemy.travel(time:single);
var
  distance: single;
begin
  distance:=speed*time;
  x:=x+distance;

  if x>maxx then
  begin //don't bother with the overlap, this way it's easier for the player
    speed:=-speed;
    x:=maxx;
  end;

  if x<minx then
  begin
    speed:=-speed;
    x:=minx;
  end;
end;

function TPlatformEnemy.getWidth:single;
begin
  result:=0.08;
end;

function TPlatformEnemy.getHeight:single;
begin
  result:=0.1;
end;

function TPlatformEnemy.getTexture: integer;
begin
  result:=ftexture;
end;

constructor TPlatformEnemy.create;
var img: tpicture;
  pp: pointer;
begin
  inherited create;
  CollisionType:=ctUnrotatedRectangles;
  if instances=0 then
  begin
    glGenTextures(1, @ftexture);

    img:=tpicture.Create;
    img.LoadFromFile(assetsfolder+'platformenemy.png');

    glBindTexture(GL_TEXTURE_2D, ftexture);
    glActiveTexture(GL_TEXTURE0);

    pp:=img.BITMAP.RawImage.Data;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.Width, img.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);


    img.free;

  end;

  inc(instances);

  speed:=0.0005;
end;

destructor TPlatformEnemy.destroy;
begin
  dec(instances);
  if instances=0 then //destroy the texture
    glDeleteTextures(1,@ftexture);

  inherited destroy;
end;


end.

