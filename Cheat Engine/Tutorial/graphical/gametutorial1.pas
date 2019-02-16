unit GameTutorial1;


//tutorial step1: player and target
//the player has an ammo capacity of  5 bullets, and the target heals itself each time the player reloads

//the task: destroy the target.  (e.g add more bullets, make the bullets do more damage, change to code to instant kill, jump to the success code, ...)


{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, gamepanel, guitextobject, staticguiobject, gamebase, player,
  target, bullet,Dialogs, Graphics;

type
  TGame1=class(TGameBase)
  private
    fpanel: Tgamepanel;
    player: TPlayer;

    target: Ttarget;
    bullets: array[0..4] of Tbullet; //max 5 bullets on the screen at once

    reloading: qword;
    reloadingtargetstarthp: integer;
    shotsfired: integer;

    lastshot: qword;
    rotatedirection: single;

    status: TGUITextObject;

    info: TGUITextObject;
    infobutton: TStaticGUIObject;



    function infoPopup(sender: tobject): boolean;
    function HideInfo(sender: tobject): boolean;
  public
    procedure gametick(currentTime: qword; diff: integer); override;
    procedure render; override;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; override;
    constructor create(p: TGamePanel);
    destructor destroy; override;
  end;

implementation

uses registry;

function TGame1.KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
var
  x: boolean;
  i: integer;
  ct: qword;


begin
  if iskeydown(VK_W) and iskeydown(VK_I) and iskeydown(VK_N) then
  begin
    usedcheats:=true;
    if target<>nil then
      target.explode; //blow up target

    exit;
  end;


  if keventtype=0 then
  begin
    ct:=GetTickCount64;



    if key=vk_space then
    begin
      if reloading<>0 then exit;

      if ct<lastshot+100 then exit; //rate limit the amount of bullets

      x:=false;
      for i:=0 to 4 do
        if bullets[i]=nil then
        begin
          //create a bullet
          bullets[i]:=tbullet.create(player);
          bullets[i].x:=player.x;
          bullets[i].y:=player.y;
          bullets[i].rotation:=player.rotation;
          x:=true;
          inc(shotsfired);
          lastshot:=ct;

          status.text:=format('Ammo till reload:'#13#10'%d',[5-shotsfired]);

          if shotsfired=5 then  //this ends up being extremely shitty giving the player hope he can win by timning it right. (not gonna happen lol)
          begin

            reloading:=ct;

            if target<>nil then
            begin
              reloadingtargetstarthp:=target.health;
              target.shielded:=true;
            end;

            status.text:='<RELOADING>';
            shotsfired:=0;
           // showmessage('reloading');
          end;
          break;
        end;


    end
    else
    begin
      case key of
        VK_LEFT,VK_A: if RotateDirection>=0 then rotatedirection:=-0.1;
        VK_RIGHT,VK_D: if RotateDirection<=0 then rotatedirection:=+0.1;

      end;
    end;
  end
  else
  begin
    case key of
      VK_LEFT,VK_A: if RotateDirection<0 then rotatedirection:=0;
      VK_RIGHT,VK_D: if RotateDirection>0 then rotatedirection:=0;
    end;
  end;
  result:=false;
end;

procedure TGame1.render;
var i: integer;
begin
  player.render;
  if target<>nil then
    target.render;

  for i:=0 to 4 do
    if bullets[i]<>nil then
      bullets[i].render;

  if info<>nil then
    info.render
  else
  begin
    if infobutton<>nil then
      infobutton.render;
  end;

  status.render;
end;

procedure tgame1.gametick(currentTime: qword; diff: integer);
var
  i: integer;
begin
  if ticking=false then exit;

  if reloading<>0 then
  begin
    if target<>nil then
      target.health:=reloadingtargetstarthp+trunc((currenttime-reloading)/2000*(100-reloadingtargetstarthp));

    //check if done reloading
    if currenttime>(reloading+2000) then
    begin
      status.text:='Ammo till reload:'#13#10'5';
      reloading:=0;

      target.health:=100;
      target.shielded:=false;
    end;
  end;

  if player<>nil then
  begin
    player.rotation:=player.rotation+rotatedirection*diff;
  end;

  for i:=0 to 4 do
    if bullets[i]<>nil then
    begin
      bullets[i].travel(diff);

      if (target<>nil) and (target.isdead=false) and bullets[i].checkCollision(target) then //perhaps use a vector based on old x,y and new x,y
      begin
        if reloading=0 then
          target.health:=target.health-24;

        if target.health<=0 then
          target.explode;

        freeandnil(bullets[i]);
      end;

      if (bullets[i]<>nil) and ((bullets[i].x>1) or (bullets[i].y>1) or (bullets[i].x<-1) or (bullets[i].y<-1)) then
      begin
        freeandnil(bullets[i]);
        //exit;
      end;
    end;

  if (target<>nil) and target.isdead and (target.blownup) then
  begin
    freeandnil(target);

    ticking:=false;
    showmessage('well done');

    with tregistry.create do
    begin
      if OpenKey('\Software\Cheat Engine\GTutorial', true) then
        WriteBool('This does not count as a solution for tutorial 1',True);

      free;
    end;


    gamewon();
  end;
end;

function TGame1.infoPopup(sender: tobject): boolean;
begin
  if info<>nil then exit(false);

  info:=TGUITextObject.create(fpanel);
  info.firstTextBecomesMinWidth:=true;
  info.width:=0.8;
  info.height:=0.8;
  info.x:=0;
  info.y:=0;
  info.rotationpoint.x:=0;
  info.rotationpoint.y:=0;
  info.color:=clBlack;
  info.bcolor:=clWhite;
  info.backgroundAlpha:=190;
  info.font.Size:=9;
  info.text:='Step 1:'#13#10'Every 5 shots you have to reload,'#13#10'after which the target will heal'#13#10'Try to find a way to destroy the target'#13#10' '#13#10'(Click to hide)';


  info.OnClick:=@HideInfo;
  result:=true;
end;

function TGame1.HideInfo(sender: tobject): boolean;
begin
  freeandnil(info);
  result:=true;
end;


destructor TGame1.destroy;
begin
  if player<>nil then
    freeandnil(player);

  if target<>nil then
    freeandnil(target);

  if status<>nil then
    freeandnil(status);

  if infobutton<>nil then
    freeandnil(infobutton);

  if info<>nil then
    freeandnil(info);

  inherited destroy;
end;

constructor TGame1.create(p: TGamePanel);
begin
  fpanel:=p;
  player:=tplayer.create;
  player.x:=0;
  player.y:=0.8;

  target:=TTarget.create;
  target.x:=0;
  target.y:=-0.8;
  target.health:=100;

  status:=TGUITextObject.create;
  status.firstTextBecomesMinWidth:=true;
  status.font.Size:=78;

  status.width:=0.4;
  status.height:=0.3;
  status.x:=1-status.width;
  status.y:=1-status.height;

  status.textalignment:=taCenter;
  status.firstTextBecomesMinWidth:=true;
  status.color:=clred;
  status.bcolor:=clgreen;

  status.text:='Ammo till reload:'#13#10'5';

  infobutton:=TStaticGUIObject.create(p,'infobutton.png',0.1,0.1);
  infobutton.rotationpoint.y:=1;  //so the bottom will be the y pos
  infobutton.x:=-1;
  infobutton.y:=1;

  infobutton.OnClick:=@infopopup;
  infopopup(infobutton);

  ticking:=true; //start



end;


end.

