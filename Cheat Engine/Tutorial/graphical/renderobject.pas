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
    fTextureCoords: record
      x: single;
      y: single;
      x2: single;
      y2: single;
    end;
  protected
    fChildrenOnly: boolean;
    procedure renderRelative; virtual;

    function getWidth:single; virtual; abstract;
    procedure setWidth(w: single); virtual;
    function getHeight:single; virtual; abstract;
    procedure setHeight(h: single); virtual;
    function getTexture: integer; virtual; abstract;

    function getLeft: single; virtual;
    function getRight: single; virtual;
    function getTop: single; virtual;
    function getBottom: single; virtual;
  public
    valid: boolean;
    x,y: single;
    rotationpoint: TPointF;

    rotation: single;

    procedure render; virtual;
    procedure addChild(child: TRenderObject);
    procedure removeChild(child: TRenderObject);
    procedure setTextureCoords(_x: single; _y: single; _x2: single; _y2: single);
    constructor create;

    property width: single read getWidth write setWidth;
    property height: single read getHeight write setHeight;
    property left: single read getLeft;
    property right: single read getRight;
    property top: single read getTop;
    property bottom: single read getBottom;

    property texture: integer read getTexture;
    property childrenonly: boolean read fChildrenOnly write fChildrenOnly;
  end;


implementation

function TRenderObject.getLeft: single;
begin
  //todo: deal with rotation  (or just let the caller deal with that)
  result:=x-width*((rotationpoint.x+1)/2);
end;

function TRenderObject.getRight: single;
begin
  //rx=-1 : x+0   |  x+w*0
  //rx=0 :  x+w/2 |  x+w*0.5
  //rx=1 :  x+w/1 |  x+w*1

  //(rx+1)/2:
  //-1 -> (-1+1)/2=0/2=0
  //0 -> (0+1)/2=1/2=0.5
  //1 -> (1+1)/2=2/2=1
  result:=x+width*((rotationpoint.x+1)/2);
end;

function TRenderObject.getTop: single;
begin
  result:=y-height*((rotationpoint.y+1)/2);
end;

function TRenderObject.getBottom: single;
begin
  result:=y+height*((rotationpoint.y+1)/2);
end;


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

procedure TRenderObject.setTextureCoords(_x: single; _y: single; _x2: single; _y2: single);
begin
  fTextureCoords.x:=_x;
  fTextureCoords.y:=_y;
  fTextureCoords.x2:=_x2;
  fTextureCoords.y2:=_y2;
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


  if fChildrenOnly=false then
  begin
    glBegin(GL_QUADS);              // Each set of 4 vertices form a quad


    glTexCoord2f(fTextureCoords.x,fTextureCoords.y2);
    glVertex2f(rx+dw*-1, ry+dh*-1);

    glTexCoord2f(fTextureCoords.x2,fTextureCoords.y2);
    glVertex2f(rx+dw, ry+dh*-1);

    glTexCoord2f(fTextureCoords.x2,fTextureCoords.y);
    glVertex2f(rx+dw, ry+dh);

    glTexCoord2f(fTextureCoords.x,fTextureCoords.y);
    glVertex2f(rx+dw*-1, ry+dh);

    glEnd();
  end;

  //render children
  for i:=0 to length(children)-1 do
  begin
    glPushMatrix();
    children[i].renderRelative;
    glPopMatrix();
  end;
end;

procedure TRenderObject.render;
begin
  glLoadIdentity();
  glPushMatrix();

  renderRelative();

  glPopMatrix();
end;

constructor TRenderObject.create;
begin
  fTextureCoords.x:=0;
  fTextureCoords.y:=0;
  fTextureCoords.x2:=1;
  fTextureCoords.y2:=1;

end;

end.

