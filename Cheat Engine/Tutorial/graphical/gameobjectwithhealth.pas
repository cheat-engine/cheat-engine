unit gameobjectwithhealth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, renderobject, math, gameobject, healthbar, gl, GLext;

type
  TGameObjectWithHealth=class(TGameObject)
  private
    fHealth: integer;
    fmaxhealth: integer;
    healthbar: THealthbar;
    fShowHealthbar: boolean;
    procedure setShowHealthbar(state: boolean);
  protected
    procedure renderRelative; override;
  public
    procedure explode; override;
    function isdead: boolean;

    procedure Damage(amount: integer);
    destructor destroy; override;
    property Health: integer read fHealth write fHealth;
    property MaxHealth: integer read fMaxHealth write fMaxHealth;
    property showHealthbar: boolean read fShowHealthbar write setShowHealthbar;
  end;

implementation

function TGameObjectWithHealth.isdead: boolean;
begin
  result:=health<=0;
end;

procedure TGameObjectWithHealth.explode;
begin
  if fhealth>=0 then
    fhealth:=0;

  if healthbar<>nil then
  begin
    removeChild(healthbar);
    freeandnil(healthbar);
  end;
  inherited;
end;

procedure TGameObjectWithHealth.renderRelative;
begin
  inherited;


  if healthbar<>nil then
  begin
    glRotatef(rotation,0,0,0.5); //undo rotation
    healthbar.percentage:=fhealth/fmaxhealth*100;
    healthbar.render;
  end;
end;

procedure TGameObjectWithHealth.setShowHealthbar(state: boolean);
begin
  fShowHealthbar:=state;
  if state then
  begin
    if healthbar=nil then
      healthbar:=THealthBar.create;

    healthbar.width:=width;
    healthbar.height:=0.05;
    healthbar.y:=(-(height/2))-(healthbar.height/2);
  end
  else
  begin
    if healthbar<>nil then
      freeandnil(healthbar);
  end;
end;

procedure TGameObjectWithHealth.Damage(amount: integer);
begin
  fhealth:=fhealth-amount;
end;

destructor TGameObjectWithHealth.destroy;
begin
  if healthbar<>nil then
    freeandnil(healthbar);

  inherited destroy;
end;

end.

