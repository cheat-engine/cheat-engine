unit GameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGameBase=class(TObject)
  private
  public
    procedure gametick(currentTime: qword; diff: integer); virtual;
    procedure render; virtual;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; virtual;
  end;

implementation

function TGameBase.KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
begin
  result:=false;
end;

procedure TGameBase.gametick(currentTime: qword; diff: integer);
begin
end;

procedure TGameBase.render;
begin
end;


end.

