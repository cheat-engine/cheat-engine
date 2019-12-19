unit gamecube;

//a rectangle you can collide with/stand on

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,glu,glext, gameobject;

type
  TGameCube=class(TGameobject)
  private
    w, h: single;
  protected
    procedure renderRelative; override;
    function getWidth:single; override;
    function getHeight:single; override;
    procedure setWidth(nv: single); override;
    procedure setHeight(nv: single); override;
  public
    color: record
      r,g,b: single;
    end;

    constructor create;
  end;

implementation

procedure TGameCube.renderRelative;
var dw,dh: single;
begin
  glDisable(GL_TEXTURE_2D);    //just gonna be lazy for this last tutorial

  dw:=w / 2;
  dh:=h / 2;

  glTranslatef(x,-y,0);

  glColor3f(color.r,color.g,color.b);
  glBegin(GL_QUADS);              // Each set of 4 vertices form a quad
  glVertex2f(dw*-1, dh*-1);
  glVertex2f(dw, dh*-1);
  glVertex2f(dw, dh);
  glVertex2f(dw*-1, dh);
  glEnd();

  glEnable(GL_TEXTURE_2D);
  glColor4f(1,1,1,1);
end;

function TGameCube.getWidth:single;
begin
  result:=w;
end;

function TGameCube.getHeight:single;
begin
  result:=h;

end;

procedure TGameCube.setWidth(nv: single);
begin
  w:=nv;
end;

procedure TGameCube.setHeight(nv: single);
begin
  h:=nv;

end;

constructor TGameCube.create;
begin
  CollisionType:=ctUnrotatedRectangles;
  inherited create;
end;


end.

