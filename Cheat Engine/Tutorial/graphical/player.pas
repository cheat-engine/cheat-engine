unit player;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,glext,glu, globals, gameobject, renderobject, graphics;

type
  TPlayer=class(TGameObject)
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
    constructor create;
    destructor destroy; override;
  end;


implementation

function TPlayer.getWidth:single;
begin
  result:=0.2; //fwidth;
end;

function TPlayer.getHeight:single;
begin
  result:=0.2*1.52; //height
end;

function TPlayer.getTexture: integer;
begin
  result:=ftexture;
end;

constructor TPlayer.create;
var img: tpicture;
  pp: pointer;
begin
  inherited create;
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

  end;

  inc(instances);
end;

destructor TPlayer.destroy;
begin
  dec(instances);
  if instances=0 then //destroy the texture
    glDeleteTextures(1,@ftexture);

  inherited destroy;
end;


end.

