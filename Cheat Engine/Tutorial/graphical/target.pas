unit target;

{$mode objfpc}{$H+}

//target practive object

interface

uses
  Classes, SysUtils, gl,glext,glu, gameobject, renderobject, graphics, healthbar,
  globals;

type
  TTarget=class(TGameObject)  //tutorial step 1. Targets have their own health (so not a TGameObjectWithHealth)
  private
    instances: integer; static;
    ftextures: record
      normal: integer;
      shielded: integer;
    end; static;
    fwidth: single; static;
    fheight: single; static;

    fShielded: boolean;

    healthbar: THealthbar;
    fhealth: integer;
    maxhealth: integer;
    procedure setHealth(h: integer);
  protected
    function getWidth:single; override;
    function getHeight:single; override;
    function getTexture: integer; override;
  public
    procedure explode; override;
    function isdead: boolean;
    property health: integer read fhealth write setHealth;
    property shielded: boolean read fShielded write fShielded;
    constructor create;
    destructor destroy; override;
  end;


implementation

procedure TTarget.explode;
begin
  fhealth:=0;
  removeChild(healthbar);
  freeandnil(healthbar);
  inherited explode;
end;

function TTarget.isdead:boolean;
var h: integer;
begin
  h:=health;
  if h<=0 then result:=true else result:=false;
end;

procedure TTarget.setHealth(h: integer);
begin
  fhealth:=h;
  if healthbar<>nil then
    healthbar.percentage:=fhealth/maxhealth*100;
end;

function TTarget.getWidth:single;
begin
  result:=0.25; //fwidth;
end;

function TTarget.getHeight:single;
begin
  result:=0.25; //*1.6; //fheight;
end;

function TTarget.getTexture: integer;
begin
  if fshielded then
    result:=ftextures.shielded
  else
    result:=ftextures.normal;


end;

constructor TTarget.create;
var img: tpicture;
  pp: pointer;
begin
  inherited create;
  if instances=0 then
  begin
    glGenTextures(2, @ftextures);

    img:=tpicture.Create;
    img.LoadFromFile(assetsfolder+'target.png');

    glBindTexture(GL_TEXTURE_2D, ftextures.normal);
    glActiveTexture(GL_TEXTURE0);

    pp:=img.BITMAP.RawImage.Data;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.Width, img.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    img.LoadFromFile(assetsfolder+'shieldedtarget.png');

    glBindTexture(GL_TEXTURE_2D, ftextures.shielded);
    glActiveTexture(GL_TEXTURE0);

    pp:=img.BITMAP.RawImage.Data;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.Width, img.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);


    img.free;

  end;

  inc(instances);

  healthbar:=thealthbar.create;
  healthbar.width:=width;
  healthbar.height:=0.05;
  healthbar.y:=(-(height/2))-(healthbar.height/2);

  self.addChild(healthbar);

  maxhealth:=100;
  health:=100;
end;

destructor TTarget.destroy;
begin
  dec(instances);
  if instances=0 then //destroy the texture
    glDeleteTextures(2,@ftextures);


  if healthbar<>nil then
    freeandnil(healthbar);

  inherited destroy;
end;


end.

