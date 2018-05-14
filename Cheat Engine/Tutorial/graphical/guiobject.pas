unit guiobject;

//not part of the game mechanics, but handles click events and wraps basic gui stuff like text

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, renderobject, gamepanel, types;

type
  TNotifyEventF=function(sender: TObject): boolean of object;
  TGUIObject=class(TRenderObject)  //abstract
  private
    fOwner: TGamePanel;

  protected
    fOnClick: TNotifyEventF;
    function getWidth:single; override;
    function getHeight:single; override;
    function getTopLeftCorner: tpointf;
    function mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean; virtual;
  public
    constructor create(owner: TGamePanel=nil; zpos: integer=-1);
    destructor destroy; override;
    property OnClick: TNotifyEventF read fOnClick write fOnClick;
  end;

implementation

function TGUIObject.getTopLeftCorner: TPointF;
begin
  //only functions when no rotation is applied
  result.x:=x-(width/2)*(rotationpoint.x+1);
  result.y:=y-(height/2)*(rotationpoint.y+1);
end;

function TGUIObject.getWidth:single;
begin
  result:=2;
end;

function TGUIObject.getHeight:single;
begin
  result:=2;
end;

function TGUIObject.mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean;
var gamepos, objectpos: tpointf;
begin
  if meventtype=0 then
  begin
    gamepos:=TGamePanel(sender).PixelPosToGamePos(mx,my);
    objectpos:=getTopLeftCorner;

    if (gamepos.x>=objectpos.x) and (gamepos.x<objectpos.x+width) and (gamepos.y>=objectpos.y) and (gamepos.y<objectpos.y+height) then
    begin
      if assigned(fOnClick) then
        exit(fOnClick(self));
    end;
  end;

  result:=false;
end;

constructor TGUIObject.create(owner: TGamePanel; zpos: integer);
begin
  fowner:=owner;
  if owner<>nil then
    owner.AddMouseEventHandler(@mhandler, zpos);

  inherited create;
end;

destructor TGUIObject.destroy;
begin
  if fowner<>nil then
    fowner.RemoveMouseEventHandler(@mhandler);

  inherited destroy;
end;

end.

