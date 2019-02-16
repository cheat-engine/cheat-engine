unit GameTutorial3;

{$mode objfpc}{$H+}

//step3:
//a platformer where everything is 1 hit kill

//Warning: While it's tempting to read through the code to find out how to beat this, try it first without



interface

uses
  windows, Classes, SysUtils, gamepanel, guitextobject, staticguiobject, gamebase,
  target, bullet,Dialogs, Graphics, playerwithhealth, math, gl, glext, glu,
  gamecube, platformenemy, platformplayer;

type

  Tdoor=class(TObject)
  private
    w: single;
    h: single;

    fx,fy: single;
    flocked: boolean;
    door: TStaticGUIObject;
    lock: TStaticGUIObject;
  public
    function isinside(_x,_y: single): boolean;
    procedure render;
    constructor create;
    destructor destroy; override;
    property locked: boolean read flocked write flocked;
    property x: single read fx write fx;
    property y: single read fy write fy;
  end;



  TGame3=class(TGameBase)
  private
    fpanel: Tgamepanel;
    pplayer: TPlatformPlayer;
    platforms: array of TgameCube;
    enemies: array of TPlatformEnemy; //change from gamecube to gameobject after testing


    info: TGUITextObject;
    infobutton: TStaticGUIObject;

    pausescreen: TGUITextObject;
    pausebutton: TStaticGUIObject;

    door: Tdoor;

    gametime: qword;

    walkdirection: single;

    jumpkeydown: boolean;
    yvelocity: single;
    falling: boolean;

    fuckplayer: boolean; //when true the platformenemies will move to form a barrier around the door
    fuckplayertime: qword;

    procedure fuckplayercode; //code to call each gametick when the game has been won

    function pauseclick(sender: tobject): boolean;
    function infoPopup(sender: tobject): boolean;
    function HideInfo(sender: tobject): boolean;
    procedure spawnPlayer;
    procedure resetgame;
  public
    greencount: integer;
    procedure gametick(currentTime: qword; diff: integer); override;
    procedure render; override;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; override;
    constructor create(p: TGamePanel);
    destructor destroy; override;
  end;

implementation

uses registry;

function TDoor.isinside(_x,_y: single): boolean;
begin
  result:=(_x>x - w/2) and
          (_x<x + w/2) and
          (_y>y - h/2) and
          (_y<y + h/2);
end;

procedure TDoor.render;
begin
  door.x:=fx;
  door.y:=fy;
  lock.x:=fx;
  lock.y:=fy;

  door.render;
  if flocked then lock.render;
end;

destructor TDoor.destroy;
begin
  freeandnil(door);
  freeandnil(lock);
  inherited destroy;
end;

constructor TDoor.create;
begin
  flocked:=true;
  w:=0.08;
  h:=0.18;
  door:=TStaticGUIObject.create(nil,'door.png',h,w);
  lock:=TStaticGUIObject.create(nil,'lock.png',h,w);

  door.rotationpoint.x:=0;
  door.rotationpoint.y:=0;

  lock.rotationpoint.x:=0;
  lock.rotationpoint.y:=0;

  x:=1-(w/2);
  y:=1-(h/2)-0.05;
end;

function TGame3.KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
var
  x: boolean;
  i: integer;
  bulletpos: integer;
  ct: qword;
begin
  if (pplayer=nil) or (pplayer.blownup) then exit;

  if iskeydown(VK_W) and iskeydown(VK_I) and iskeydown(VK_N) then
  begin
    usedcheats:=true;
    for i:=0 to length(enemies)-1 do
      enemies[i].explode;

    door.locked:=false;
    exit;
  end;

  if iskeydown(VK_D) and iskeydown(VK_I) and iskeydown(VK_E) then
  begin
    pplayer.explode;
    exit;
  end;

  if iskeydown(VK_F) and iskeydown(VK_C) and iskeydown(VK_K) then
  begin
    door.locked:=false;
    fuckplayer:=true;
    fuckplayertime:=gametime;
    exit;
  end;

//  if player.isdead then exit;

  if keventtype=0 then
  begin
    ct:=GetTickCount64;

    case key of
      VK_P: pauseclick(nil);
      VK_LEFT,VK_A: if walkdirection>=0 then walkdirection:=-0.1; //starts with a lot of inertia
      VK_RIGHT,VK_D: if walkdirection<=0 then walkdirection:=0.1;
      VK_SPACE, VK_W,VK_UP:
      begin
        jumpkeydown:=true; //makes mountaingoating easier
        if falling=false then
        begin
          yvelocity:=1.45;
        end;
      end;
    end;

  end
  else
  begin
    case key of
      VK_LEFT,VK_A: if walkdirection<0 then walkdirection:=0;
      VK_RIGHT,VK_D: if walkdirection>0 then walkdirection:=0;
      VK_SPACE,VK_W,VK_UP:
      begin
        jumpkeydown:=false;
      end;
    end;
  end;


  result:=false;
end;

procedure TGame3.render;
var i: integer;
begin
  for i:=0 to length(platforms)-1 do
    platforms[i].render;

  if info<>nil then
    info.render
  else
  begin
    if infobutton<>nil then
      infobutton.render;
  end;

  pausebutton.render;
  if pausescreen<>nil then
    pausescreen.render;

  if pplayer<>nil then
    pplayer.render;

  for i:=0 to length(enemies)-1 do
  begin
    if enemies[i]<>nil then
      enemies[i].render;
  end;

  if door<>nil then
    door.render;
end;

procedure TGame3.fuckplayercode;
//in 2 seconds take positions
var
  t: qword;
  pos: single;

  targetx, targety: single;

  distance: single;
begin
  t:=gametime-fuckplayertime;
  pos:=t/2000;

  if length(enemies)>=3 then
  begin
    if enemies[0]<>nil then
    begin
      //move it to the bottom left rotated 45 degress to the left
      //take 2 seconds to rotate

      targetx:=door.x-(door.w/2)-(enemies[0].width/2);

      if pos>=1 then
      begin
        enemies[0].rotation:=360-90;
        enemies[0].x:=targetx;
      end
      else
      begin
        enemies[0].rotation:=360-(90*pos);

        distance:=targetx-enemies[0].x;
        enemies[0].x:=enemies[0].x+distance*pos;
      end;
    end;

    if enemies[1]<>nil then
    begin
      //move it to the topleft rotated 45 degrees to the left
      targetx:=door.x-(door.w/2)-(enemies[0].width/2);
      targety:=enemies[0].y-(enemies[0].height/2)-(enemies[1].height/2);
      if pos>=1 then
      begin
        enemies[1].rotation:=360-90;
        enemies[1].x:=targetx;
        enemies[1].y:=targety;
      end
      else
      begin
        enemies[1].rotation:=360-(90*pos);
        distance:=targetx-enemies[1].x;
        enemies[1].x:=enemies[1].x+distance*pos;

        distance:=targety-enemies[1].y;
        enemies[1].y:=enemies[1].y+distance*pos;
      end;
    end;

    if enemies[2]<>nil then
    begin
      //move it to the topright
      targetx:=door.x;
      targety:=door.y-(door.h/2)-(enemies[2].width/2);

      if pos>=1 then
      begin
        enemies[2].x:=targetx;
        enemies[2].y:=targety;
      end
      else
      begin
        distance:=targetx-enemies[2].x;
        enemies[2].x:=enemies[2].x+distance*pos;

        distance:=targety-enemies[2].y;
        enemies[2].y:=enemies[2].y+distance*pos;
      end;
    end;

  end;
end;

procedure TGame3.gametick(currentTime: qword; diff: integer);
var
  i,j: integer;
  oldx, oldy: single;
  newx, newy: single;
  oldplayerfeet, newplayerfeet: single;
  platformwalky: single;

begin
  if ticking=false then exit;

  if pausescreen<>nil then
    exit;

  inc(gametime,diff);

  oldx:=pplayer.x;
  oldy:=pplayer.y;

  newy:=pplayer.y-(yvelocity*(diff/1000));


  oldplayerfeet:=oldy+pplayer.height/2;
  newplayerfeet:=newy+pplayer.height/2;

  pplayer.y:=newy;
  if yvelocity<0 then
  begin
    //falling down
    //check the platforms that are between oldy and pplayer.y
    for i:=0 to length(platforms)-1 do
    begin
      platformwalky:=platforms[i].y-(platforms[i].height/2);
      if (oldplayerfeet<=platformwalky) and
         (newplayerfeet>=platformwalky) then
      begin
        //it passed this platform. Check if the width is good enough


        if (pplayer.x>platforms[i].x-platforms[i].width/2) and
           (pplayer.x<platforms[i].x+platforms[i].width/2) then
        begin
          //yes, it landed on top
          if platforms[i].color.g=0 then
            inc(greencount); //I could of course check if all are green, but this provides an easy hackable spot to speed things up

          platforms[i].color.r:=0;
          platforms[i].color.g:=1;

          falling:=false;
          yvelocity:=0;
          pplayer.y:=platformwalky-pplayer.height/2; //put it on top of the platform
          break;
        end;
      end;
    end;
  end;

  if yvelocity<>0 then
    falling:=true;

  if jumpkeydown and not falling then
  begin
    yvelocity:=1.45
  end
  else
    yvelocity:=yvelocity-0.1;




  if walkdirection<>0 then
    walkdirection:=walkdirection*1.035;

  if walkdirection<-0.7 then walkdirection:=-0.7;
  if walkdirection>0.7 then walkdirection:=0.7;


  if (pplayer<>nil) and (walkdirection<>0) then
  begin
    pplayer.x:=pplayer.x+(walkdirection*(diff/1000));

  end;

  if pplayer.x>(1-pplayer.width/2) then pplayer.x:=(1-pplayer.width/2);
  if pplayer.x<(-1+pplayer.width/2) then pplayer.x:=(-1+pplayer.width/2);

  if fuckplayer then
    fuckplayercode
  else
  begin
    for i:=0 to length(enemies)-1 do
    begin
      if enemies[i]<>nil then
        enemies[i].travel(diff);
    end;
  end;

  for i:=0 to length(enemies)-1 do
  begin
    if (enemies[i]<>nil) then
    begin
      if (not enemies[i].exploding) and enemies[i].checkCollision(pplayer) then
        pplayer.explode;

      if enemies[i].blownup then
        freeandnil(enemies[i]);
    end;
  end;

  if (door.locked) and (greencount>=length(platforms)) then
  begin
    door.locked:=false;
    fuckplayer:=true; //what happens when you nop this out ?
    fuckplayertime:=gametime; //another point of attack
  end;
  //I 'could' do a constant check for door.unlocked and then enter the fuckplayer mode, but just being nice and making it possible to unlock the door without having to mark all green



  if (pplayer<>nil) and (not pplayer.exploding) and (door.locked=false) and door.isinside(pplayer.x,pplayer.y) then
  begin
    ticking:=false;
    showmessage('well done');
    with tregistry.create do
    begin
      if OpenKey('\Software\Cheat Engine\GTutorial', true) then
        WriteBool('This does not count as a solution for tutorial 3',True);

      free;
    end;
    gamewon;
  end;

  if (pplayer<>nil) and (pplayer.blownup) then
  begin
    //recreate the player
    //game over, restart
    freeandnil(pplayer);
    resetgame;
  end;
end;

function TGame3.pauseclick(sender: tobject): boolean;
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

function TGame3.infoPopup(sender: tobject): boolean;
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
  info.text:='Step 3:'#13#10+
             'Mark all platforms green to'#13#10+
             'unlock the door'#13#10+
             ' '#13#10+
             'Beware: Enemies are 1 hit kill'#13#10+
             '        (and bad losers)'#13#10+
             ' '#13#10+
             ' '#13#10+
             'Have fun!'#13#10+
             ' '#13#10+
             'Hint: There are multiple solutions'#13#10+
             ' e.g: Find collisiton detect with '#13#10+
             '      enemies, or teleport, or fly'#13#10+
             '      or ....'#13#10;

  info.OnClick:=@HideInfo;
  result:=true;
end;

function TGame3.HideInfo(sender: tobject): boolean;
begin
  freeandnil(info);
  result:=true;
end;

procedure TGame3.spawnPlayer;
begin
  if pplayer=nil then
    pplayer:=tplatformplayer.create; //temporary holder

  pplayer.x:=-1+0.1;
  pplayer.y:=1-0.05-0.15/2;
end;

procedure TGame3.resetgame;
var i: integer;
begin
  spawnplayer;

  if length(platforms)<12 then
    setlength(platforms,12);

  for i:=0 to length(platforms)-1 do
  begin
    if platforms[i]=nil then
      platforms[i]:=Tgamecube.create;

    platforms[i].color.b:=0;
    platforms[i].color.r:=0.8;
    platforms[i].color.g:=0;
  end;

  //ground
  platforms[0].width:=2;
  platforms[0].height:=0.05;
  platforms[0].x:=0;
  platforms[0].y:=0.975;

  //p1
  platforms[1].width:=0.3;
  platforms[1].height:=0.05;
  platforms[1].x:=-0.4;
  platforms[1].y:=0.7;



  platforms[2].width:=0.3;
  platforms[2].height:=0.05;
  platforms[2].x:=0.1;
  platforms[2].y:=0.7;


  platforms[3].width:=0.2;
  platforms[3].height:=0.05;
  platforms[3].x:=0;
  platforms[3].y:=0.42;



  platforms[4].width:=0.2;
  platforms[4].height:=0.05;
  platforms[4].x:=0.4;
  platforms[4].y:=0.2;

  platforms[5].width:=0.2;
  platforms[5].height:=0.05;
  platforms[5].x:=0.6;
  platforms[5].y:=-0.05;

  platforms[6].width:=0.4;
  platforms[6].height:=0.05;
  platforms[6].x:=0.75;
  platforms[6].y:=-0.3;

  platforms[7].width:=0.1;
  platforms[7].height:=0.05;
  platforms[7].x:=0.0;
  platforms[7].y:=-0.3;

  platforms[8].width:=0.4;
  platforms[8].height:=0.05;
  platforms[8].x:=-0.3;
  platforms[8].y:=-0.5;

  platforms[9].width:=0.2;
  platforms[9].height:=0.05;
  platforms[9].x:=-0.4;
  platforms[9].y:=-0.7;

  platforms[10].width:=0.2;
  platforms[10].height:=0.05;
  platforms[10].x:=-0.8;
  platforms[10].y:=-0.7;

  platforms[11].width:=0.1;
  platforms[11].height:=0.05;
  platforms[11].x:=-0.8;
  platforms[11].y:=0;

  fuckplayer:=false;

  if length(enemies)<3 then
    setlength(enemies,3);

  if enemies[0]=nil then
    enemies[0]:=TPlatformEnemy.create;

  enemies[0].x:=-0.5;
  enemies[0].y:=1-enemies[0].height/2-platforms[0].height;
  enemies[0].minx:=-0.6;
  enemies[0].maxx:=0-enemies[0].width/2;
  enemies[0].rotation:=0;


  if enemies[1]=nil then
    enemies[1]:=TPlatformEnemy.create;

  enemies[1].x:=0+enemies[0].width/2;
  enemies[1].y:=1-enemies[1].height/2-platforms[2].height;
  enemies[1].minx:=0;
  enemies[1].maxx:=0.9;
  enemies[1].rotation:=0;

  if enemies[2]=nil then
    enemies[2]:=TPlatformEnemy.create;

  enemies[2].x:=0+enemies[0].width/2;
  enemies[2].y:=platforms[8].y-(enemies[2].height/2)-(platforms[8].height/2);
  enemies[2].minx:=platforms[8].x-platforms[8].width/2;
  enemies[2].maxx:=platforms[8].x+platforms[8].width/2;
  enemies[2].speed:=enemies[1].speed/2; //got to keep the illusion the player can potentially win

  greencount:=0;

  door.locked:=true;
end;

destructor TGame3.destroy;
begin
  if pplayer<>nil then
    freeandnil(pplayer);

  if infobutton<>nil then
    freeandnil(infobutton);

  if info<>nil then
    freeandnil(info);

  inherited destroy;
end;

constructor TGame3.create(p: TGamePanel);
var i: integer;
begin
  fpanel:=p;

  infobutton:=TStaticGUIObject.create(p,'infobutton.png',0.1,0.1);
  infobutton.rotationpoint.y:=1;  //so the bottom will be the y pos
  infobutton.x:=-1;
  infobutton.y:=1-0.05;
  infobutton.OnClick:=@infopopup;

  pausebutton:=TStaticGUIObject.create(p,'pausebutton.png',0.1,0.1);
  pausebutton.rotationpoint.y:=1;
  pausebutton.x:=-1+0.1;
  pausebutton.y:=1-0.05;
  pausebutton.OnClick:=@pauseclick;

  door:=Tdoor.create;

  resetgame;

  infopopup(infobutton);
  ticking:=true;

 { p.background.r:=0;
  p.background.g:=0;
  p.background.b:=1;}
end;


end.

