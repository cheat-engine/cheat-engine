unit GameTutorial2;

{$mode objfpc}{$H+}

//step2:
//2 targets, that shoot at the player as well.
//targets and player share the same health decreasing code

interface

uses
  windows, Classes, SysUtils, gamepanel, guitextobject, staticguiobject, gamebase,
  target, bullet,Dialogs, Graphics, playerwithhealth, math;

type
  TGame2=class(TGameBase)
  private
    fpanel: Tgamepanel;
    player: TPlayerWithHealth;
    target1,target2: TPlayerWithHealth;

    bullets: array of Tbullet; //max 5 bullets on the screen at once

    shotsfired: integer;

    lastshot: qword;
    rotatedirection: single;

    usemegabombs: boolean;

    speedup: boolean;
    speed: single;

    status: TGUITextObject;

    info: TGUITextObject;
    infobutton: TStaticGUIObject;

    pausescreen: TGUITextObject;
    pausebutton: TStaticGUIObject;

    enemytext: TGUITextObject;
    enemytextStartTime: qword;

    gametime: qword;

    function getNextBulletPos: integer;
    function pauseclick(sender: tobject): boolean;
    function infoPopup(sender: tobject): boolean;
    function HideInfo(sender: tobject): boolean;
    procedure spawnPlayer;
    procedure spawnTarget1;
    procedure spawnTarget2;
  public
    procedure SpawnEnemyText;
    procedure gametick(currentTime: qword; diff: integer); override;
    procedure render; override;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; override;
    constructor create(p: TGamePanel);
    destructor destroy; override;
  end;

implementation

uses registry;

function TGame2.getNextBulletPos: integer;
var i: integer;
begin
  for i:=0 to length(bullets)-1 do
    if bullets[i]=nil then exit(i);

  result:=length(bullets);
  if result=0 then
    setlength(bullets,8)
  else
    setlength(bullets,length(bullets)*2);
  for i:=result to length(bullets)-1 do
    bullets[i]:=nil;
end;

function TGame2.KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
var
  x: boolean;
  i: integer;
  bulletpos: integer;
  ct: qword;
begin
  if iskeydown(VK_W) and iskeydown(VK_I) and iskeydown(VK_N) then
  begin
    usedcheats:=true;
    target1.explode;
    target2.explode;
    exit;
  end;

  if iskeydown(VK_D) and iskeydown(VK_I) and iskeydown(VK_E) then
  begin
    rotatedirection:=0;
    player.explode;
    exit;
  end;

  if player.isdead then exit;

  if keventtype=0 then
  begin
    ct:=GetTickCount64;

    if key=vk_space then
    begin
      if ct<lastshot+100 then exit; //rate limit the amount of bullets

      i:=getNextBulletPos;

      //create a bullet
      bullets[i]:=tbullet.create(player);
      bullets[i].x:=player.x;
      bullets[i].y:=player.y;
      bullets[i].rotation:=player.rotation;
      bullets[i].damage:=1;

      lastshot:=ct;

      //let the enemies shoot as well
      //target the player pos
      if target1<>nil then
        target1.rotation:=((arctan2(player.y-target1.y, player.x-target1.x))*(180)/pi)+90;

      if target2<>nil then
        target2.rotation:=((arctan2(player.y-target2.y, player.x-target2.x))*(180)/pi)+90;

      if target1<>nil then
      begin
        i:=getNextBulletPos;

        bullets[i]:=tbullet.create(target1);
        bullets[i].x:=target1.x;
        bullets[i].y:=target1.y;
        bullets[i].rotation:=target1.rotation;
        bullets[i].damage:=2;

        if usemegabombs then
        begin
          bullets[i].speed:=bullets[i].speed*0.4;
          bullets[i].megabomb:=true;
          bullets[i].damage:=player.Health+1;
        end;
      end;


      if target2<>nil then
      begin
        i:=getNextBulletPos;
        bullets[i]:=tbullet.create(target2);
        bullets[i].x:=target2.x;
        bullets[i].y:=target2.y;
        bullets[i].rotation:=target2.rotation;
        bullets[i].damage:=2;

        if usemegabombs then
        begin
          bullets[i].speed:=bullets[i].speed*0.4;
          bullets[i].megabomb:=true;
          bullets[i].damage:=player.Health+1;
        end;
      end;

    end
    else
    begin
      case key of
        VK_LEFT,VK_A: if RotateDirection>=0 then rotatedirection:=-0.1*(ifthen(ssShift in shift, 3,1));
        VK_RIGHT,VK_D: if RotateDirection<=0 then rotatedirection:=+0.1*(ifthen(ssShift in shift, 3,1));
        VK_UP, VK_W: speedup:=true;
        VK_DOWN, VK_S: speedup:=false;
      end;
    end;
  end
  else
  begin
    case key of
      VK_LEFT,VK_A: if RotateDirection<0 then rotatedirection:=0;
      VK_RIGHT,VK_D: if RotateDirection>0 then rotatedirection:=0;
      vk_UP,VK_W: speedup:=false;
      vk_DOWN,VK_S: speedup:=false;
    end;
  end;
  result:=false;
end;

procedure TGame2.render;
var i: integer;
begin
  player.render;
  if target1<>nil then
    target1.render;

  if target2<>nil then
    target2.render;

  for i:=0 to length(bullets)-1 do
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

  pausebutton.render;
  if pausescreen<>nil then
    pausescreen.render;

  if enemytext<>nil then
    enemytext.render;
end;

procedure TGame2.gametick(currentTime: qword; diff: integer);
var i,j: integer;
  megabombsecleft: integer;
begin
  if ticking=false then exit;

  if pausescreen<>nil then
    exit;

  inc(gametime,diff);

  if player<>nil then
  begin
    if speedup then
    begin
      speed:=speed+0.0001;
      if speed>0.003 then speed:=0.003;

    end
    else
    begin
      if speed>0 then
        speed:=speed-0.0002;

      if speed<0 then speed:=0;
    end;

    player.rotation:=player.rotation+rotatedirection*diff;
    player.x:=player.x+(diff*speed)*sin(degtorad(player.rotation));
    player.y:=player.y+(diff*speed)*-cos(degtorad(player.rotation));

    if player.x>1 then player.x:=1;
    if player.x<-1 then player.x:=-1;
    if player.y>1 then player.y:=1;
    if player.y<-1 then player.y:=-1;
  end;
  //adjust targets to point to player
  //....

  if target1<>nil then
    target1.rotation:=((arctan2(player.y-target1.y, player.x-target1.x))*(180)/pi)+90;

  if target2<>nil then
    target2.rotation:=((arctan2(player.y-target2.y, player.x-target2.x))*(180)/pi)+90;



  for i:=0 to length(bullets)-1 do
    if bullets[i]<>nil then
    begin
      if bullets[i].megabomb then
      begin
        bullets[i].rotation:=((arctan2(player.y-bullets[i].y, player.x-bullets[i].x))*(180)/pi)+90;
        bullets[i].speed:=bullets[i].speed+0.00001*(diff/100);
      end;

      bullets[i].travel(diff);

      if bullets[i].checkCollision(player) then
      begin
        player.damage(bullets[i].damage);


        if player.isdead then
        begin
          player.explode;

          //remove all bullets
          for j:=0 to length(bullets)-1 do
            freeandnil(bullets[j]);
        end;

        status.text:=format('Player Health: '#13#10'%d',[player.health]);


        if bullets[i]<>nil then
          freeandnil(bullets[i]);
      end;

      //and yes, you can let them shoot eachother (except megabombs)

      if (bullets[i]<>nil) and (bullets[i].megabomb=false) and (target1<>nil) and (target1.isdead=false) and bullets[i].checkCollision(target1) then //perhaps use a vector based on old x,y and new x,y
      begin
        target1.damage(bullets[i].damage);

        if target1.isdead then
        begin
          target1.explode;
          if (target2<>nil) and (target2.isdead=false) then
          begin
            spawnEnemyText;
            target2.health:=min(target2.health+20, target2.MaxHealth);  //just increase the level of assholerly a little bit more
          end;
        end;

        freeandnil(bullets[i]);
      end;


      if (bullets[i]<>nil) and (bullets[i].megabomb=false) and (target2<>nil) and (target2.isdead=false) and bullets[i].checkCollision(target2) then //perhaps use a vector based on old x,y and new x,y
      begin
        target2.damage(bullets[i].damage);

        if target2.isdead then
        begin
          target2.explode;
          if (target1<>nil) and (target1.isdead=false) then
          begin
            spawnEnemyText;
            target1.health:=min(target1.health+20, target1.MaxHealth);
          end;
        end;

        freeandnil(bullets[i]);
      end;

      if (bullets[i]<>nil) and ((bullets[i].x>1) or (bullets[i].y>1) or (bullets[i].x<-1) or (bullets[i].y<-1)) then
      begin
        freeandnil(bullets[i]);
        exit;
      end;
    end;


  if enemytext<>nil then
  begin
    megabombsecleft:=ceil(integer((3000-(gametime-enemytextStartTime)))/1000);
    enemytext.Text:='You will pay for this!'#13#10'Activating megabombs'#13#10''+inttostr(megabombsecleft);
    if megabombsecleft<0 then
    begin
      if target1<>nil then
        target1.enemycharged:=true;

      if target2<>nil then
        target2.enemycharged:=true;

      usemegabombs:=true;
      freeandnil(enemytext);
    end;
  end;


  if (target1<>nil) and target1.isdead and (target1.blownup) then
    freeandnil(target1);

  if (target2<>nil) and target2.isdead and (target2.blownup) then
    freeandnil(target2);


  if (player<>nil) and player.isdead and (player.blownup) then
  begin
    //recreate the player
    //game over, restart
    usemegabombs:=false;
    if target1<>nil then
      target1.enemycharged:=false;

    if target2<>nil then
      target2.enemycharged:=false;

    freeandnil(player);
    spawnplayer;

    if target1=nil then
      spawnTarget1;

    target1.health:=200;

    if target2=nil then
      spawnTarget2;

    target2.health:=200;

    status.text:=format('Player Health: '#13#10'%d',[player.health]);
  end;

  if (target1=nil) and (target2=nil) then
  begin
    ticking:=false;
    showmessage('well done');

    with tregistry.create do
    begin
      if OpenKey('\Software\Cheat Engine\GTutorial', true) then
        WriteBool('This does not count as a solution for tutorial 2',True);

      free;
    end;

    gamewon();
    exit;
  end;

end;

procedure TGame2.SpawnEnemyText;
begin
  enemytext:=TGUITextObject.create(nil);
  enemytext.firstTextBecomesMinWidth:=true;
  enemytext.width:=0.8;
  enemytext.height:=0.8;
  enemytext.x:=0;
  enemytext.y:=0;
  enemytext.rotationpoint.x:=0;
  enemytext.rotationpoint.y:=0;
  enemytext.color:=clRED;
  enemytext.bcolor:=clWhite;
  enemytext.font.Size:=18;
  enemytext.Text:='You will pay for this!'#13#10'Activating megabombs'#13#10''+inttostr(3);
  enemytextStartTime:=gametime;
end;

function TGame2.pauseclick(sender: tobject): boolean;
begin
  if pausescreen<>nil then
    freeandnil(pausescreen)
  else
  begin
    pausescreen:=TGUITextObject.create(fpanel);
    pausescreen.firstTextBecomesMinWidth:=true;
    pausescreen.width:=0.8;
    pausescreen.height:=0.8;
    pausescreen.x:=0;
    pausescreen.y:=0;
    pausescreen.rotationpoint.x:=0;
    pausescreen.rotationpoint.y:=0;
    pausescreen.color:=clRED;
    pausescreen.bcolor:=clWhite;
    pausescreen.font.Size:=18;
    pausescreen.text:='!PAUSED!';
  end;
  result:=true;
end;

function TGame2.infoPopup(sender: tobject): boolean;
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
  info.text:='Step 2:'#13#10+
             'These two enemies have more health'#13#10+
             'and do more damage to you than'#13#10+
             'you do to them. Destroy them'#13#10+
             ' '#13#10+
             'Tip/Warning: Enemy and player are related'#13#10+
             ' '#13#10'(Click to hide)';

  info.OnClick:=@HideInfo;
  result:=true;
end;

function TGame2.HideInfo(sender: tobject): boolean;
begin
  freeandnil(info);
  result:=true;
end;

procedure TGame2.spawnPlayer;
begin
  player:=TPlayerWithHealth.create(false);
  player.x:=0;
  player.y:=0.8;
  player.health:=100;
  player.maxhealth:=100;
end;

procedure TGame2.spawnTarget1;
begin
  target1:=TPlayerWithHealth.create(true);
  target1.x:=0-target1.width-0.005;
  target1.y:=-0.8;
  target1.health:=200;
  target1.maxhealth:=200;
  target1.rotation:=180;
end;

procedure TGame2.spawnTarget2;
begin
  target2:=TPlayerWithHealth.create(true);
  target2.x:=target2.width+0.005;
  target2.y:=-0.8;
  target2.health:=200;
  target2.maxhealth:=200;
  target2.rotation:=180;
end;

destructor TGame2.destroy;
begin
  if player<>nil then
    freeandnil(player);

  if target1<>nil then
    freeandnil(target1);

  if target2<>nil then
    freeandnil(target2);


  if status<>nil then
    freeandnil(status);

  if infobutton<>nil then
    freeandnil(infobutton);

  if info<>nil then
    freeandnil(info);

  inherited destroy;
end;

constructor TGame2.create(p: TGamePanel);
begin
  fpanel:=p;

  spawnPlayer;
  spawnTarget1;
  spawnTarget2;


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

  status.text:='Player Health: '#13#10'100';

  infobutton:=TStaticGUIObject.create(p,'infobutton.png',0.1,0.1);
  infobutton.rotationpoint.y:=1;  //so the bottom will be the y pos
  infobutton.x:=-1;
  infobutton.y:=1;
  infobutton.OnClick:=@infopopup;

  pausebutton:=TStaticGUIObject.create(p,'pausebutton.png',0.1,0.1);
  pausebutton.rotationpoint.y:=1;
  pausebutton.x:=-1+0.1;
  pausebutton.y:=1;
  pausebutton.OnClick:=@pauseclick;

  infopopup(infobutton);
  ticking:=true;
end;


end.

