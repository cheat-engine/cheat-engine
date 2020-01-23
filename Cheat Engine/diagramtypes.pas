unit diagramtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls{$ifdef windows}, GL{$endif};

type
  TDiagramBlockSide=(dbsTop, dbsLeft, dbsRight, dbsBottom, dbsTopLeft, dbsTopRight, dbsBottomLeft, dbsBottomRight);

  TArrowStyle=(asOrigin, asDestination, asPoints, asCenterBetweenPoints, asCenter); //centerbetweenpoints and asCenter will eat you cpu
  TArrowStyles=set of TArrowStyle;


  TDiagramConfig=class
  public
    owner: TCustomControl;
    canvas: TCanvas;
    LineThickness: integer;
    LineColor: TColor;
    PlotPointColor: TColor;
    PlotPointSize: integer;
    backgroundColor: TColor;
    drawPlotPoints: boolean;

    blockTextColorNoMarkup: tcolor;
    BlockBackground: tcolor;

    arrowStyles: TArrowStyles;
    arrowSize: integer;
    scrollx, scrolly, maxscrollx, maxscrolly: integer;
    zoom: single;

    {$ifdef windows}
    UseOpenGL: boolean;
    {$endif}
    CanUsebuffers: boolean;


    {$ifdef windows}
    plotpointvertexbuf: GLint;
    plotpointindexbuf: GLint;
    {$endif}


    constructor create(_owner: TCustomControl);
  end;

implementation

constructor TDiagramConfig.create(_owner: TCustomControl);
begin
  owner:=_owner;
  canvas:=_owner.Canvas;
  LineThickness:=3;
  LineColor:=clBlack;
  PlotPointColor:=3;
  PlotPointSize:=4;
  backgroundcolor:=clGreen;
  drawPlotPoints:=true;
  BlockBackground:=$d0d0d0;

  arrowsize:=5;

  blockTextColorNoMarkup:=clBlack;
end;

end.

