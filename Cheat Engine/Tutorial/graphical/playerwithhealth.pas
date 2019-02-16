unit playerwithhealth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,glext,glu, globals, gameobject, renderobject, graphics,
  gameobjectwithhealth, healthbar;

type
  TPlayerWithHealth=class(TGameObjectWithHealth)
  private
    instances: integer; static;
    ftexture: integer; static;
    ftextureEnemy: integer; static;
    ftextureEnemyCharged: integer; static;
    fwidth: single; static;
    fheight: single; static;
    fEnemyCharged: boolean;
    isenemy: boolean; //just to make it easier for people who want to hack this
  protected
    function getWidth:single; override;
    function getHeight:single; override;
    function getTexture: integer; override;
  public
    property enemycharged: boolean read fEnemyCharged write fEnemyCharged ;
    constructor create(enemy: boolean);
    destructor destroy; override;

  end;


implementation

function TPlayerWithHealth.getWidth:single;
begin
  result:=0.2; //fwidth;
end;

function TPlayerWithHealth.getHeight:single;
begin
  result:=0.2*1.52; //height
end;

function TPlayerWithHealth.getTexture: integer;
begin
  if isenemy then
  begin
    if fEnemyCharged then
      result:=ftextureEnemyCharged
    else
      result:=ftextureEnemy;
  end
  else
    result:=ftexture;
end;



constructor TPlayerWithHealth.create(enemy: boolean);
var img: tpicture;
  pp: pointer;
begin
  inherited create;

  isenemy:=enemy;
  if instances=0 then
  begin
    glGenTextures(1, @ftexture);

    img:=tpicture.Create;
    img.LoadFromFile(assetsfolder+'xxx.png');


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

    glGenTextures(1, @ftextureEnemy);

    img:=tpicture.Create;
    img.LoadFromFile(assetsfolder+'xxx2.png');


    glBindTexture(GL_TEXTURE_2D, ftextureEnemy);
    glActiveTexture(GL_TEXTURE0);

    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 2, 2, 0, GL_RGB, GL_FLOAT, @pixels[0]);

    pp:=img.BITMAP.RawImage.Data;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.Width, img.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    img.free;


    glGenTextures(1, @ftextureEnemyCharged);

    img:=tpicture.Create;
    img.LoadFromFile(assetsfolder+'xxx3.png');


    glBindTexture(GL_TEXTURE_2D, ftextureEnemyCharged);
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

  inc(instances);

  ShowHealthbar:=isenemy;
end;

destructor TPlayerWithHealth.destroy;
begin
  dec(instances);
  if instances=0 then //destroy the textures
  begin
    glDeleteTextures(1,@ftexture);
    glDeleteTextures(1,@ftextureEnemy);
    glDeleteTextures(1,@ftextureEnemyCharged);
  end;

  inherited destroy;
end;


end.

