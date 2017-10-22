unit guiobject;

//not part of the game mechanics, but handles click events and wraps basic gui stuff like text

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, renderobject, gamepanel;

type
  TGuiObject=class(TRenderObject)
  private
    fOwner: TGamePanel;
  protected
    function getWidth:single; override;
    function getHeight:single; override;

    function mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean; virtual;
  public
    constructor create(owner: TGamePanel=nil; zpos: integer=-1);
  end;

implementation

function TGuiObject.getWidth:single;
begin
  result:=2;
end;

function TGuiObject.getHeight:single;
begin
  result:=2;
end;

function TGuiObject.mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean;
begin
  //default behavior: Check the loaded image. Check the alpha channel value. if >50% transparant then do not handle it

  //get the x,y coordinate of the render object


  result:=false;
end;

constructor TGuiObject.create(owner: TGamePanel; zpos: integer);
begin
  if owner<>nil then
    owner.AddMouseEventHandler(@mhandler, zpos);

  inherited create;
end;

end.

