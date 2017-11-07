unit renderobject;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils,GL, GLext, glu, types;

type
  TRenderObject=class(TObject)
  private
    children: array of TRenderObject;
  protected
    procedure renderRelative; virtual;

    function getWidth:single; virtual; abstract;
    procedure setWidth(w: single); virtual;
    function getHeight:single; virtual; abstract;
    procedure setHeight(h: single); virtual;
    function getTexture: integer; virtual; abstract;
  public
    valid: boolean;
    x,y: single;
    rotationpoint: TPointF;

    rotation: single;

    procedure render; virtual;
    procedure addChild(child: TRenderObject);
    procedure removeChild(child: TRenderObject);

    property width: single read getWidth write setWidth;
    property height: single read getHeight write setHeight;
    property texture: integer read getTexture;
  end;


implementation

procedure TRenderObject.setWidth(w: single);
begin
  //
end;

procedure TRenderObject.setHeight(h: single);
begin
  //
end;

procedure TRenderObject.addChild(child: TRenderObject);
begin
  setlength(children, length(children)+1);
  children[length(children)-1]:=child;
end;

procedure TRenderObject.removeChild(child: TRenderObject);
var i,j: integer;
begin
  for i:=0 to length(children)-1 do
    if children[i]=child then
    begin
      for j:=i to length(children)-2 do
        children[j]:=children[j+1];

      setlength(children, length(children)-1);
    end;
end;

procedure TRenderObject.renderRelative;
var w, h: single;

  dw,dh: single;

  rx,ry: single;
  i: integer;
begin
  //set the texture
  glBindTexture(GL_TEXTURE_2D, getTexture);
  glActiveTexture(GL_TEXTURE0);

  w:=width; // / 2;
  h:=height;// / 2;

  dw:=width / 2;
  dh:=height / 2;

  glTranslatef(x,-y,0);
  glRotatef(-rotation,0,0,0.5);


  rx:=-rotationpoint.x;
  ry:=-rotationpoint.y;

  if rx<>0 then
    rx:=dw*rx;

  if ry<>0 then
    ry:=-1*dh*ry;



  glBegin(GL_QUADS);              // Each set of 4 vertices form a quad


  glTexCoord2f(0,1);
  glVertex2f(rx+dw*-1, ry+dh*-1);

  glTexCoord2f(1,1);
  glVertex2f(rx+dw, ry+dh*-1);

  glTexCoord2f(1,0);
  glVertex2f(rx+dw, ry+dh);

  glTexCoord2f(0,0);
  glVertex2f(rx+dw*-1, ry+dh);


  glEnd();

  //render children
  for i:=0 to length(children)-1 do
    children[i].renderRelative;
end;

procedure TRenderObject.render;
begin
  glLoadIdentity();
  glPushMatrix();

  renderRelative();

  glPopMatrix();
end;

end.

