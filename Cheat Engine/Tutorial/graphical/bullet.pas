unit bullet;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, gameobject, graphics, gl, glext, math, globals;

type
  TBullet=class(TGameObject)
  private
    instances2: integer; static;
    ftexture2: integer; static;
    fowner: TGameObject;
    fmegabomb: boolean;
  protected
    function getWidth:single; override;
    function getHeight:single; override;
    function getTexture: integer; override;
  public
    speed: single; //distance / ms
    damage: integer;
    function checkCollision(other: TGameObject): boolean; override;

    property megabomb: boolean read fmegabomb write fmegabomb;
    procedure travel(time: single);
    constructor create(owner: TGameObject);
    destructor destroy; override;
  end;


implementation

function TBullet.checkCollision(other: TGameObject): boolean;
begin
  if other=fowner then
    result:=false
  else
    result:=inherited checkCollision(other);
end;

procedure TBullet.travel(time:single);
var
  distance: single;
  d: single;
begin
  distance:=speed*time;

  x:=x+distance*sin(degtorad(rotation));
  y:=y+distance*-cos(degtorad(rotation));

  //todo, check for collision here instead

end;

function TBullet.getWidth:single;
begin
  result:=0.05; //fwidth;
  if megabomb then result:=result*3;
end;

function TBullet.getHeight:single;
begin
  result:=0.05*1.6; //height
  if megabomb then result:=result*3;
end;

function TBullet.getTexture: integer;
begin
  result:=ftexture2;
end;

constructor TBullet.create(owner: TGameObject);
var img: tpicture;
  pp: pointer;
begin
  inherited create;

  fowner:=owner;

  speed:=0.001;
  if instances2=0 then
  begin
    glGenTextures(1, @ftexture2);

    img:=tpicture.Create;
    img.LoadFromFile(assetsfolder+'bullet.png');

    glBindTexture(GL_TEXTURE_2D, ftexture2);
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

  inc(instances2);
end;

destructor TBullet.destroy;
begin
  dec(instances2);
  if instances2=0 then //destroy the texture
    glDeleteTextures(1,@ftexture2);

  inherited destroy;
end;


end.

