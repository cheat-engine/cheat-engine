unit GameTutorial3;

{$mode objfpc}{$H+}

//step3:
//a platformer where everything is 1 hit kill

interface

uses
  windows, Classes, SysUtils, gamepanel, guitextobject, staticguiobject, gamebase,
  target, bullet,Dialogs, Graphics, playerwithhealth, math, gl, glext, glu, gamecube;

type
  TGame3=class(TGameBase)
  private
    fpanel: Tgamepanel;
    pplayer: TGameCube;
    platforms: array of TgameCube;


    info: TGUITextObject;
    infobutton: TStaticGUIObject;

    pausescreen: TGUITextObject;
    pausebutton: TStaticGUIObject;

    gametime: qword;

    walkdirection: single;

    yvelocity: single;
    falling: boolean;


    function pauseclick(sender: tobject): boolean;
    function infoPopup(sender: tobject): boolean;
    function HideInfo(sender: tobject): boolean;
    procedure spawnPlayer;
  public
    procedure gametick(currentTime: qword; diff: integer); override;
    procedure render; override;
    function KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean; override;
    constructor create(p: TGamePanel);
    destructor destroy; override;
  end;

implementation


function TGame3.KeyHandler(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean;
var
  x: boolean;
  i: integer;
  bulletpos: integer;
  ct: qword;
begin
  if iskeydown(VK_W) and iskeydown(VK_I) and iskeydown(VK_N) then
  begin
    usedcheats:=true;
    exit;
  end;

  if iskeydown(VK_D) and iskeydown(VK_I) and iskeydown(VK_E) then
  begin
    pplayer.explode;
    exit;
  end;

//  if player.isdead then exit;

  if keventtype=0 then
  begin
    ct:=GetTickCount64;

    case key of
      VK_LEFT,VK_A: if walkdirection>=0 then walkdirection:=-0.1; //starts with a lot of inertia
      VK_RIGHT,VK_D: if walkdirection<=0 then walkdirection:=0.1;
      VK_SPACE, VK_W,VK_UP:
      begin
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

{  for i:=0 to length(platforms)-1 do
  begin
    if platforms[i].checkCollision(pplayer) then
    begin
      pplayer.x:=oldx;
    end;
  end;  }


  if (pplayer<>nil) and (pplayer.blownup) then
  begin
    //recreate the player
    //game over, restart
    freeandnil(pplayer);
    spawnplayer;
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
             'Get to the finish. Enemies will'#13#10+
             'insta-kill you, and some jumps'#13#10+
             'are impossible'#13#10+
             'Have fun!'#13#10+
             #13#10+
             'Hint: There are multiple solutions'#13#10+
             ' e.g: Ultimap1/2 or teleport, or fly'#13#10+
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
  pplayer:=tgamecube.create; //temporary holder
  pplayer.width:=0.06;
  pplayer.height:=0.15;
  pplayer.color.r:=0;
  pplayer.color.g:=1;
  pplayer.color.b:=0.1;
  pplayer.x:=-1+0.1;
  pplayer.y:=1-0.05-0.15/2;
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

  setlength(platforms,11);
  for i:=0 to length(platforms)-1 do
  begin
    platforms[i]:=Tgamecube.create;
    platforms[i].color.b:=0;
    platforms[i].color.r:=0.8;
    platforms[i].color.g:=0.1;
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

  platforms[8].width:=0.2;
  platforms[8].height:=0.05;
  platforms[8].x:=-0.2;
  platforms[8].y:=-0.5;

  platforms[9].width:=0.2;
  platforms[9].height:=0.05;
  platforms[9].x:=-0.4;
  platforms[9].y:=-0.7;

  platforms[10].width:=0.2;
  platforms[10].height:=0.05;
  platforms[10].x:=-0.8;
  platforms[10].y:=-0.7;

  spawnplayer;

  infopopup(infobutton);
  ticking:=true;

 { p.background.r:=0;
  p.background.g:=0;
  p.background.b:=1;}
end;


end.

