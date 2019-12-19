unit particle;
//a non game (GUI) object that takes it's texture from a pre-existing texture id

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, guiobject, gl, GLext;

type
  TParticle=class(TGuiObject)
  private
    ftexture: integer;
    fwidth: single;
    fheight: single;
  protected
    function getWidth:single; override;
    function getHeight:single; override;
    procedure setWidth(w: single); override;
    procedure setHeight(h: single); override;

    function getTexture: integer; override;
  public
    constructor create(t: integer);
  end;

implementation

procedure TParticle.setWidth(w: single);
begin
  fwidth:=w;
end;

procedure TParticle.setHeight(h: single);
begin
  fheight:=h;
end;

function TParticle.getWidth:single;
begin
  result:=fwidth;
end;

function TParticle.getHeight:single;
begin
  result:=fheight;
end;

function TParticle.getTexture: integer;
begin
  result:=ftexture;
end;

constructor TParticle.create(t: integer);
begin
  fwidth:=0.1;
  fheight:=0.1;
  fTexture:=t;
  inherited create;
end;

end.

