unit LevelSelect;


//tutorial step1: player and target
//the player has an ammo capacity of  5 bullets, and the target heals itself each time the player reloads

//the task: destroy the target.  (e.g add more bullets, make the bullets do more damage, change to code to instant kill, jump to the success code, ...)


{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, gamepanel, guitextobject, staticguiobject, gamebase, player,
  target, bullet,Dialogs, Graphics;

type
  TLevelSelect=class(TGameBase)
  private
    fpanel: Tgamepanel;
    player: TPlayer;

    target1: Ttarget;
    target2: Ttarget;
    target3: Ttarget;
    bullets: array[0..4] of Tbullet; //max 5 bullets on the screen at once

    lastshot: qword;
    rotatedirection: single;

    info: TGUITextObject;
    infobutton: TStaticGUIObject;

    l1text, l2text, l3text: TGUITextObject;



    function infoPopup(sender: tobject): boolean;
    function HideInfo(sender: tobject): boolean;
  public
    level: integer;
    procedure gametick(currentTime: qword; diff: integer); override;
    procedure render; override;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; override;
    constructor create(p: TGamePanel);
    destructor destroy; override;
  end;

implementation

uses registry;

function TLevelSelect.KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
var
  x: boolean;
  i: integer;
  ct: qword;


begin
  if iskeydown(VK_W) and iskeydown(VK_I) and iskeydown(VK_N) then
  begin
    showMessage('You lose instead!');
    ExitProcess(0);
  end;


  if keventtype=0 then
  begin
    ct:=GetTickCount64;



    if key=vk_space then
    begin
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
          lastshot:=ct;
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

procedure TLevelSelect.render;
var i: integer;
begin
  player.render;
  if target1<>nil then target1.render;
  if target2<>nil then target2.render;
  if target3<>nil then target3.render;

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

  if l1text<>nil then l1text.render;
  if l2text<>nil then l2text.render;
  if l3text<>nil then l3text.render;

end;

procedure TLevelSelect.gametick(currentTime: qword; diff: integer);
var
  i,j: integer;
begin
  if ticking=false then exit;

  if player<>nil then
    player.rotation:=player.rotation+rotatedirection*diff;

  for i:=0 to 4 do
    if bullets[i]<>nil then
    begin
      bullets[i].travel(diff);


      if (target1<>nil) and (target1.isdead=false) and bullets[i].checkCollision(target1) then //perhaps use a vector based on old x,y and new x,y
      begin
        target1.health:=target1.health-24;

        if target1.health<=0 then
          target1.explode;

        freeandnil(bullets[i]);
        continue;
      end;

      if (target2<>nil) and (target2.isdead=false) and bullets[i].checkCollision(target2) then //perhaps use a vector based on old x,y and new x,y
      begin
        target2.health:=target2.health-24;

        if target2.health<=0 then
          target2.explode;

        freeandnil(bullets[i]);
        continue;
      end;

      if (target3<>nil) and (target3.isdead=false) and bullets[i].checkCollision(target3) then //perhaps use a vector based on old x,y and new x,y
      begin
        target3.health:=target3.health-24;

        if target3.health<=0 then
          target3.explode;

        freeandnil(bullets[i]);
        continue;
      end;

      if (bullets[i]<>nil) and ((bullets[i].x>1) or (bullets[i].y>1) or (bullets[i].x<-1) or (bullets[i].y<-1)) then
      begin
        freeandnil(bullets[i]);
        //exit;
      end;
    end;

  if (target1<>nil) and target1.isdead and (target1.blownup) then
  begin
    freeandnil(target1);
    level:=1;
    ticking:=false;
    gamewon();
    exit;
  end;

  if (target2<>nil) and target2.isdead and (target2.blownup) then
  begin
    freeandnil(target2);
    level:=2;
    ticking:=false;
    gamewon();
    exit;
  end;

  if (target3<>nil) and target3.isdead and (target3.blownup) then
  begin
    freeandnil(target3);
    level:=3;
    ticking:=false;
    gamewon();
    exit;
  end;
end;

function TLevelSelect.infoPopup(sender: tobject): boolean;
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
  info.text:='Level Select:'#13#10'Destroy the target for the specific level.'#13#10' '#13#10'(Click to hide)';


  info.OnClick:=@HideInfo;
  result:=true;
end;

function TLevelSelect.HideInfo(sender: tobject): boolean;
begin
  freeandnil(info);
  result:=true;
end;


destructor TLevelSelect.destroy;
begin
  if player<>nil then
    freeandnil(player);

  if target1<>nil then
    freeandnil(target1);

  if target2<>nil then
    freeandnil(target2);

  if target3<>nil then
    freeandnil(target3);

  if infobutton<>nil then
    freeandnil(infobutton);

  if info<>nil then
    freeandnil(info);

  if l1text<>nil then
    l1text.destroy;

  if l2text<>nil then
    l2text.destroy;

  if l3text<>nil then
    l3text.destroy;

  inherited destroy;
end;

constructor TLevelSelect.create(p: TGamePanel);
var reg: tregistry;
begin
  fpanel:=p;
  player:=tplayer.create;
  player.x:=0;
  player.y:=0.8;

  target1:=TTarget.create;
  target1.x:=-0.4;
  target1.y:=-0.8;
  target1.health:=100;

  l1text:=TGUITextObject.create(p);


  l1text.firstTextBecomesMinWidth:=true;
  l1text.textalignment:=taCenter;
  l1text.width:=target1.width;
  l1text.height:=0.1;
  l1text.x:=target1.x;
  l1text.y:=target1.y+target1.height;
  l1text.rotationpoint.x:=0;
  l1text.rotationpoint.y:=1;
  l1text.color:=clRed;
  l1text.bcolor:=clBlack;
  l1text.backgroundAlpha:=190;
  l1text.font.Size:=9;
  l1text.text:='Level 1';


  reg:=tregistry.create;
  if reg.OpenKey('\Software\Cheat Engine\GTutorial', false) then
  begin
    if reg.ValueExists('This does not count as a solution for tutorial 1') then
    begin
      target2:=TTarget.create;
      target2.x:=0;
      target2.y:=-0.8;
      target2.health:=100;

      l2text:=TGUITextObject.create(p);
      l2text.firstTextBecomesMinWidth:=true;
      l2text.textalignment:=taCenter;
      l2text.width:=target2.width;
      l2text.height:=0.1;
      l2text.x:=target2.x;
      l2text.y:=target2.y+target2.height;
      l2text.rotationpoint.x:=0;
      l2text.rotationpoint.y:=1;
      l2text.color:=clRed;
      l2text.bcolor:=clBlack;
      l2text.backgroundAlpha:=190;
      l2text.font.Size:=9;
      l2text.text:='Level 2';
    end;
  end;

  reg:=tregistry.create;
  if reg.OpenKey('\Software\Cheat Engine\GTutorial', false) then
  begin
    if reg.ValueExists('This does not count as a solution for tutorial 2') then
    begin
      target3:=TTarget.create;
      target3.x:=0.4;
      target3.y:=-0.8;
      target3.health:=100;

      l3text:=TGUITextObject.create(p);
      l3text.firstTextBecomesMinWidth:=true;
      l3text.textalignment:=taCenter;
      l3text.width:=target1.width;
      l3text.height:=0.1;
      l3text.x:=target3.x;
      l3text.y:=target3.y+target3.height;
      l3text.rotationpoint.x:=0;
      l3text.rotationpoint.y:=1;
      l3text.color:=clRed;
      l3text.bcolor:=clBlack;
      l3text.backgroundAlpha:=190;
      l3text.font.Size:=9;
      l3text.text:='Level 3';
    end;
  end;


  infobutton:=TStaticGUIObject.create(p,'infobutton.png',0.1,0.1);
  infobutton.rotationpoint.y:=1;  //so the bottom will be the y pos
  infobutton.x:=-1;
  infobutton.y:=1;

  infobutton.OnClick:=@infopopup;
  infopopup(infobutton);

  ticking:=true; //start


  level:=1;

end;


end.

