unit GameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGameBase=class(TObject)
  private
    fOnWin: TNotifyEvent;
    fticking: boolean;
  public
    function isKeyDown(key:dword): boolean;
    procedure gametick(currentTime: qword; diff: integer); virtual;
    procedure render; virtual;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; virtual;
    procedure gamewon;
    property OnWin: TNotifyEvent read fOnWin write fOnWin;
    property ticking: boolean read fticking write fticking;
  end;

var usedcheats: boolean; //secret tutorial: make this false (hint: it's a static)

implementation

uses windows;

procedure TGameBase.gamewon;
begin
  if assigned(fOnWin) then
    fOnWin(self);
end;

function TGameBase.isKeyDown(key:dword): boolean;
begin
  result:=(GetKeyState(key) and (1<<15))>0;
end;

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

