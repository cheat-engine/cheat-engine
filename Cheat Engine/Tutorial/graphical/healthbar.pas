unit healthbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, guiobject, gl, GLext;

type
  THealthBar=class(TGuiObject)
  private
    fpercentage: single;
    w, h: single;
    fborder: single;
  protected
    procedure renderRelative; override;
    function getWidth:single; override;
    function getHeight:single; override;
    procedure setWidth(nv: single); override;
    procedure setHeight(nv: single); override;

  public
    constructor create;

    procedure render; override;
    property border: single read fborder write fborder;
    property percentage: single read fpercentage write fpercentage;
  end;

implementation

function THealthBar.getWidth:single;
begin
  result:=w;
end;

function THealthBar.getHeight:single;
begin
  result:=h;
end;

procedure THealthBar.setWidth(nv: single);
begin
  w:=nv;
end;

procedure THealthBar.setHeight(nv: single);
begin
  h:=nv;
end;

procedure THealthBar.renderRelative;
var dw, dh: single;

  ry: single;
  percentagewidth: single;
begin
  //render the bar using primitives (no textures)
  glDisable(GL_TEXTURE_2D);



  dw:=w / 2;
  dh:=h / 2;

  ry:=-y;

  percentagewidth:=(w-2*fborder)*((100-fpercentage)/100);


  //outside border
  glColor3f(1,0,0);
  glBegin(GL_QUADS);              // Each set of 4 vertices form a quad

//  glTexCoord2f(0,1);
  glVertex2f(x+dw*-1, ry+dh*-1);

//  glTexCoord2f(1,1);
  glVertex2f(x+dw, ry+dh*-1);

//  glTexCoord2f(1,0);
  glVertex2f(x+dw, ry+dh);

//  glTexCoord2f(0,0);
  glVertex2f(x+dw*-1, ry+dh);
  glEnd();

  //health background
  glColor3f(0,0,1);
  glBegin(GL_QUADS);              // Each set of 4 vertices form a quad

 // glTexCoord2f(0,1);
  glVertex2f(x+dw*-1+fborder, ry+dh*-1+fborder);

 // glTexCoord2f(1,1);
  glVertex2f(x+dw-fborder, ry+dh*-1+fborder);

 // glTexCoord2f(1,0);
  glVertex2f(x+dw-fborder, ry+dh-fborder);

 // glTexCoord2f(0,0);
  glVertex2f(x+dw*-1+fborder, ry+dh-fborder);
  glEnd();


  //inside border (progress)
  glColor3f(0,1,0);
  glBegin(GL_QUADS);              // Each set of 4 vertices form a quad

 // glTexCoord2f(0,1);
  glVertex2f(x+dw*-1+fborder, ry+dh*-1+fborder);

 // glTexCoord2f(1,1);
  glVertex2f(x+dw-fborder-percentagewidth, ry+dh*-1+fborder);

 // glTexCoord2f(1,0);
  glVertex2f(x+dw-fborder-percentagewidth, ry+dh-fborder);

 // glTexCoord2f(0,0);
  glVertex2f(x+dw*-1+fborder, ry+dh-fborder);
  glEnd();

  glColor4f(1,1,1,1);

  glEnable(GL_TEXTURE_2D);
end;

procedure THealthBar.render;
begin
  renderRelative; // raise exception.create('Do not call render on THealthBar objects');
end;

constructor THealthBar.create;
begin
  w:=0.1;
  h:=0.1;
  fborder:=0.008;


  percentage:=50;

end;

end.

