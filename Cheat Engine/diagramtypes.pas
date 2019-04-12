unit diagramtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TDiagramBlockSide=(dbsTop, dbsLeft, dbsRight, dbsBottom, dbsTopLeft, dbsTopRight, dbsBottomLeft, dbsBottomRight);

  TArrowStyle=(asOrigin, asDestination, asPoints, asCenterBetweenPoints, asCenter); //centerbetweenpoints and asCenter will eat you cpu
  TArrowStyles=set of TArrowStyle;


  TDiagramConfig=class
  public
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

    constructor create(_canvas: TCanvas);
  end;

implementation

constructor TDiagramConfig.create(_canvas: TCanvas);
begin
  canvas:=_canvas;
  LineThickness:=3;
  LineColor:=clBlack;
  PlotPointColor:=3;
  PlotPointSize:=4;
  backgroundcolor:=clGreen;
  drawPlotPoints:=true;

  blockTextColorNoMarkup:=clBlack;
end;

end.

