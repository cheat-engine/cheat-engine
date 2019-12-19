program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, fpvectorialpkg, Unit1, gamepanel, renderobject, player,
  gameobject, animationobject, guiobject, scoreboard, guitextobject, target,
  bullet, guidialog, globals, staticguiobject, GameTutorial1, GameBase,
  GameTutorial2, PlayerWithHealth, gameobjectwithhealth, particle,
  movingparticle, gametutorial3, gamecube, platformenemy, LevelSelect,
  frmHelpUnit
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='CE Game tutorials';
  RequireDerivedFormResource:=True;
  Application.Initialize;

  //assetsfolder:=extractfiledir(application.ExeName)+'\badassets\';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmHelp, frmHelp);
  Application.Run;
end.

