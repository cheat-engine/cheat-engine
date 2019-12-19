unit movingparticle;

//a particle, but then with a speed and direction. Every render it will update it's position

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,glu,glext, particle, math;

type TMovingParticle=class(TParticle)
  public
    starttime: qword;
    speed: single;
    direction: single;
    initialx: single;
    initialy: single;
    procedure renderRelative; override;
end;

implementation

procedure TMovingParticle.renderRelative;
//update x and y based on the time, speed and direction

var
  distance: single;
  d: single;
begin
  glRotatef(rotation,0,0,0.5);

  distance:=speed*(gettickcount64-starttime);

  x:=initialx+distance*sin(degtorad(direction));
  y:=initialy+distance*-cos(degtorad(direction));

  inherited renderRelative;
end;

end.

