unit animationobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, renderobject;

type
  TAnimationObject=class(TRenderobject)
  {
  Render object just for drawing animations
  }
  protected
    currentTexture: integer;
    lastTextureTime: qword;
    textures: array of integer;

    function getWidth:single; override;
    function getHeight:single; override;
    function getTexture: integer; override;
  public
    speed: integer; //ms per frame
  end;

implementation

function TAnimationObject.getWidth:single;
begin
  result:=2;
end;

function TAnimationObject.getHeight:single;
begin
  result:=2;
end;

function TAnimationObject.getTexture: integer;
var t: qword;
begin
  t:=GetTickCount64;
  if t>(lasttexturetime+speed) then
  begin
    currentTexture:=(texture+1) mod length(textures);
    lastTextureTime:=t;
  end;

  result:=textures[currentTexture];
end;

end.

