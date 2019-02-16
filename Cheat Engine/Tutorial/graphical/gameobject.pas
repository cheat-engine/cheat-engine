unit gameobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, renderobject, math, movingparticle;

//abstract class

type
  TCollisionType=(ctCircles, ctUnrotatedRectangles);
  TGameObject=class(TRenderobject)
  {
  Render object with game mechanics like hitboxes and
  }
  private
    explodetime: qword;
    fIsExploding: boolean;
    fRange: single;
    fRenderCollision: boolean;
    explosion: array of TMovingParticle;


    function getRange: single;
  protected
    CollisionType: TCollisionType;
  public
    procedure explode; virtual; //splits up the objects into a million objects
    function exploding: boolean;
    function blownup: boolean; //returns true if it has gone far enough

    function checkCollision(other: TGameObject):boolean; virtual;

    property range: single read getRange;
    property renderCollision: boolean read fRenderCollision write fRenderCollision;



    destructor destroy; override;
  end;

implementation
  {
procedure TGameObject.renderRelative;
begin
  inherited renderRelative;

  if fRenderCollision then
  begin
    //todo: render collision box/sphere
  end;
end; }


function TGameObject.blownup: boolean; //returns true if it has gone far enough
begin
  if fIsExploding then
    result:=gettickcount64>=explodetime+2000
  else
    result:=false;
end;

function TGameobject.exploding: boolean;
begin
  result:=fIsExploding;
end;

procedure TGameObject.explode;
var
  t: integer;
  i: integer;
  f: single;

begin
  //boom
  //create 64 children

  if fIsExploding then exit;

  fIsExploding:=true;
  explodetime:=gettickcount64;

  setlength(explosion,64);

  fChildrenOnly:=true;
  width:=0;
  height:=0;

  t:=getTexture;
  for i:=0 to 63 do
  begin
    explosion[i]:=TMovingParticle.create(t);
    explosion[i].starttime:=explodetime;
    explosion[i].speed:=0.001+0.0001*(5+random(10));
    explosion[i].direction:=random(360);//i*8;//random(360);
    explosion[i].x:=0;
    explosion[i].initialx:=0;
    explosion[i].y:=0;
    explosion[i].initialy:=0;
    explosion[i].width:=0.02;
    explosion[i].height:=0.02;

    explosion[i].setTextureCoords((i mod 8)/8,i/8/8,(i mod 8)/8+0.125,i/8/8+0.125);

    addChild(explosion[i]);
  end;
end;

function TGameObject.getRange: single;
begin
  if fRange=0 then
    frange:=(width+height)/4; //just an average

  result:=frange;
end;

function TGameObject.checkCollision(other: TGameObject):boolean;
//only circle ranges are handled atm. So don't use very rectangular objects

//todo: Add rectangular support, including rotated rectangles
//todo2: polygon collision
//honestly though, everyone is free to add this, I only need this basic stuff for the tutorial
var
  range_other: single;
  range_self: single;
  distance: single;
begin
  result:=false;
  if CollisionType=ctCircles then
  begin
    range_other:=other.range;
    range_self:=range;
    distance:=sqrt(sqr(abs(other.x-x))+sqr(abs(other.y-y)));

    result:=distance<(range_other+range_self);
  end
  else
  if CollisionType=ctUnrotatedRectangles then
  begin
    //rectangles

    Result := (Left < other.Right) and (other.Left < Right) and (Top < other.Bottom) and (other.Top < Bottom);

  end;
end;

destructor TGameObject.destroy;
var i: integer;
begin
  for i:=0 to length(explosion)-1 do
  begin
    removeChild(explosion[i]);
    explosion[i].Free;
  end;

  setlength(explosion,0);

  inherited destroy;
end;

end.

