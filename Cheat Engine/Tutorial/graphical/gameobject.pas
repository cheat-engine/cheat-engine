unit gameobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, renderobject, math;

type
  TGameObject=class(TRenderobject)
  {
  Render object with game mechanics like hitboxes and
  }
  private
    fRange: single;
    fRenderCollision: boolean;
    function getRange: single;
  protected
  //  procedure renderRelative; override;
  public
    function checkCollision(other: TGameObject):boolean; virtual;
    property range: single read getRange;
    property renderCollision: boolean read fRenderCollision write fRenderCollision;
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
  range_other:=other.range;
  range_self:=range;
  distance:=sqrt(sqr(abs(other.x-x))+sqr(abs(other.y-y)));

  result:=distance<(range_other+range_self);
end;

end.

