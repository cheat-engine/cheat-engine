unit scoreboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, guiobject, gamepanel, controls, graphics, gl,glu,GLext,
  guitextobject, types, dialogs, globals;

type
  TScoreBoard=class(TGuiObject)
  private
    instances: integer; static;
    ftexture: integer; static;

    fscore: integer;
    health: integer;

    scoreText: TGUITextObject;
    procedure setScore(i: integer);
  protected
    function mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean; override;
    function getTexture: integer; override;
    function getWidth:single; override;
    function getHeight:single; override;
  public
    property score: integer read fScore write setScore;
    constructor create(owner: TGamePanel; zpos: integer=-1);
    destructor destroy; override;
  end;

implementation

procedure TScoreBoard.setScore(i: integer);
begin
  fscore:=i;
  scoreText.Text:=inttostr(i);
end;

function TScoreBoard.getWidth:single;
begin
  result:=0.6;
end;

function TScoreBoard.getHeight:single;
begin
  result:=0.4;
end;

function TScoreBoard.getTexture: integer;
begin
  result:=ftexture;
end;

function TScoreBoard.mhandler(sender: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; mX, mY: Integer): boolean;
var
  gamepos: TPointF;
  r: TRectF;
begin
  //convert mx,my to coordinates
  gamepos:=TGamePanel(sender).PixelPosToGamePos(mx,my);

  if meventtype=0 then
  begin
    if (gamepos.x<=x+width) and (gamepos.y<=y+height) and (gamepos.x>=x) and (gamepos.y>=y) then
      ShowMessage('click');
  end;

  result:=false;

end;

destructor TScoreBoard.destroy;
begin
  dec(instances);
  if instances=0 then
    glDeleteTextures(1,@ftexture);


  scoreText.Free;

  inherited destroy;
end;

constructor TScoreBoard.create(owner: TGamePanel; zpos: integer=-1);
var
  pp: pointer;
  p: TPortableNetworkGraphic;
begin
  inherited create(owner, zpos);

  rotationpoint.x:=-1; //top left
  rotationpoint.y:=-1;

  if instances=0 then
  begin
    glGenTextures(1, @ftexture);

    p:=TPortableNetworkGraphic.Create;
    p.LoadFromFile(assetsfolder+'scoreboard.png');


    glBindTexture(GL_TEXTURE_2D, ftexture);
    glActiveTexture(GL_TEXTURE0);

    pp:=p.RawImage.Data;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, p.Width, p.height, 0, GL_BGRA,  GL_UNSIGNED_BYTE, pp);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    p.free;
  end;
  inc(instances);

  scoreText:=TGUITextObject.create(owner);
  scoreText.font.Size:=16;

  scoretext.width:=0.4;
  scoretext.height:=0.07;

  scoreText.textalignment:=taLeft;
  scoreText.firstTextBecomesMinWidth:=true;
  scoreText.text:='1000';

  scoretext.x:=0.1;
  scoretext.y:=0.1;

  addChild(scoreText);
end;

end.

