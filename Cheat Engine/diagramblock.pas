unit diagramblock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, types, DiagramTypes, Graphics, textrender;

type
  TDiagramBlock=class;
  TDiagramBlockSideDescriptor=object
  public
    block: TDiagramBlock;
    side: TDiagramBlockSide;
    sideposition: integer; //0 is center, -1 is one pixel to the left, 1 is one pixel to the rigth
    constructor init;
  end;

  TDiagramBlock=class
  private
    fx,fy: integer;
    fwidth: integer;
    fheight: integer;

    fname: string;
    fcaption: string;

    fOnDoubleClickHeader: TNotifyEvent;
    fOnDoubleClickBody: TNotifyEvent;

    data: tstringlist;

    captionheight: integer;
    config: TDiagramConfig;
    fOnDestroy: TNotifyEvent;

    useCustomBackgroundColor: boolean;
    customBackgroundColor: tcolor;

    useCustomDefaultTextColor: boolean;
    CustomDefaultTextColor: Tcolor;

    function getBackgroundColor: TColor;
    procedure setBackgroundColor(c: TColor);
    function getDefaultTextColor: TColor;
    procedure setDefaultTextColor(c: TColor);
  public

    function getData: TStrings;
    procedure setData(s: TStrings);
    procedure dblClick(xpos,ypos: integer);

    function IsAtBorder(xpos, ypos: integer; var side: TDiagramBlockSide): boolean;
    function IsInsideHeader(xpos,ypos: integer):boolean;
    function IsInsideBody(xpos,ypos: integer):boolean;
    function IsInside(xpos,ypos: integer):boolean;
    procedure resize(xpos, ypos: integer; side: TDiagramBlockSide);
    function getConnectPosition(side: TDiagramBlockSide; position: integer=0): tpoint;
    procedure render;
    property OnDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;
    constructor create(graphConfig: TDiagramConfig);
    destructor destroy; override;
  published
    property X: integer read fx write fx;
    property Y: integer read fy write fy;
    property Width: integer read fwidth write fwidth;
    property Height: integer read fheight write fheight;
    property Caption: string read fcaption write fcaption;
    property Strings: TStrings read getData write setData;
    property BackgroundColor: TColor read getBackgroundColor write setBackgroundColor;
    property DefaultTextColor: TColor read getDefaultTextColor write setDefaultTextColor;
    property Name: string read fname write fname;
    property OnDoubleClickHeader: TNotifyEvent read fOnDoubleClickHeader write fOnDoubleClickHeader;
    property OnDoubleClickBody: TNotifyEvent read fOnDoubleClickBody write fOnDoubleClickBody;

  end;

implementation

constructor TDiagramBlockSideDescriptor.init;
begin
  sideposition:=0;
end;

function TDiagramBlock.getBackgroundColor: TColor;
begin
  if useCustomBackgroundColor then
    result:=customBackgroundColor
  else
    result:=config.BlockBackground;
end;

procedure TDiagramBlock.setBackgroundColor(c: TColor);
begin
  customBackgroundColor:=c;
  useCustomBackgroundColor:=true;
end;

function TDiagramBlock.getDefaultTextColor: TColor;
begin
  if useCustomDefaultTextColor then
    result:=CustomDefaultTextColor
  else
    result:=config.blockTextColorNoMarkup;
end;

procedure TDiagramBlock.setDefaultTextColor(c: TColor);
begin
  CustomDefaultTextColor:=c;
  useCustomDefaultTextColor:=true;
end;

procedure TDiagramBlock.render;
var
  c: TCanvas;
  oldbgc: TColor;
  oldfontcolor: TColor;
begin
  //render the block at the given location
  c:=config.canvas;

  oldbgc:=c.brush.color;
  c.brush.color:=BackgroundColor;
  c.FillRect(x,y,x+width,y+height);

  c.Rectangle(x,y,x+width,y+height);

  if captionheight=0 then
    captionheight:=c.GetTextHeight('XxYyJjQq')+4;

  c.Rectangle(x,y,x+width,y+captionheight);

  oldfontcolor:=c.font.color;
  c.font.color:=defaultTextColor;


  renderFormattedText(c, rect(x,y,x+width,y+captionheight),x+1,y,caption);
  //c.TextRect(rect(x,y,x+width,y+captionheight),x,y,caption);

  renderFormattedText(c, rect(x,y+captionheight,x+width,y+height-2),x+1,y+captionheight,data.text);
  //c.TextRect(rect(x,y+captionheight,x+width,y+height-2),x,y+captionheight,data.text);

  c.font.color:=oldfontcolor;
  c.brush.color:=oldbgc;
end;

function TDiagramBlock.getData: TStrings;
begin
  result:=data;
end;

procedure TDiagramBlock.setData(s: TStrings);
begin
  data.clear;
  data.Assign(s);
end;

procedure TDiagramBlock.dblClick(xpos,ypos: integer);
begin
  if assigned(fOnDoubleClickBody) and IsInsideBody(xpos,ypos) then
    fOnDoubleClickBody(self)
  else
  if assigned(fOnDoubleClickHeader) and IsInsideHeader(xpos,ypos) then
    fOnDoubleClickHeader(self);

end;

function TDiagramBlock.IsInsideHeader(xpos,ypos: integer):boolean;
var
  headerrect: trect;
begin
  headerrect:=rect(x,y,x+width,y+captionheight);
  result:=PtInRect(headerrect,point(xpos,ypos));
end;

function TDiagramBlock.IsInsideBody(xpos,ypos: integer):boolean;
var
  bodyrect: trect;
begin
  bodyrect:=rect(x,y+captionheight,x+width,y+height);
  result:=PtInRect(bodyrect,point(xpos,ypos));
end;

function TDiagramBlock.IsInside(xpos,ypos: integer):boolean;
var
  r: trect;
begin
  r:=rect(x,y,x+width,y+height);
  result:=PtInRect(r,point(xpos,ypos));
end;

function TDiagramBlock.IsAtBorder(xpos, ypos: integer; var side: TDiagramBlockSide): boolean;
var
  borderthickness: integer;
begin
  result:=false;
  borderthickness:=3;

  if PtInRect(rect(x-borderthickness,y-borderthickness,x+width+borderthickness,y+height+borderthickness),point(xpos,ypos))=false then exit; //not even within the range

  //still here so it's within the region
  if PtInRect(rect(x-borderthickness,y-borderthickness, x+borderthickness,y+borderthickness),point(xpos,ypos)) then
  begin
    side:=dbsTopLeft;
    exit(true);
  end;

  if PtInRect(rect(x+width-borderthickness,y-borderthickness, x+width+borderthickness,y+borderthickness),point(xpos,ypos)) then
  begin
    side:=dbsTopRight;
    exit(true);
  end;

  if PtInRect(rect(x-borderthickness,y+height-borderthickness, x+borderthickness,y+height+borderthickness),point(xpos,ypos)) then
  begin
    side:=dbsBottomLeft;
    exit(true);
  end;

  if PtInRect(rect(x+width-borderthickness,y+height-borderthickness, x+width+borderthickness,y+height+borderthickness),point(xpos,ypos)) then
  begin
    side:=dbsBottomRight;
    exit(true);
  end;

  if PtInRect(rect(x,y-borderthickness, x+width,y+borderthickness),point(xpos,ypos)) then
  begin
    side:=dbsTop;
    exit(true);
  end;

  if PtInRect(rect(x+width-borderthickness,y, x+width+borderthickness,y+height),point(xpos,ypos)) then
  begin
    side:=dbsRight;
    exit(true);
  end;

  if PtInRect(rect(x,y+height-borderthickness, x+width,y+height+borderthickness),point(xpos,ypos)) then
  begin
    side:=dbsBottom;
    exit(true);
  end;

  if PtInRect(rect(x-borderthickness,y, x+borderthickness,y+height),point(xpos,ypos)) then
  begin
    side:=dbsLeft;
    exit(true);
  end;
end;

function TDiagramBlock.getConnectPosition(side: TDiagramBlockSide; position: integer=0): tpoint;
//returns the canvas x,y position of the specified side's center, offset by the given position up to the max length of the side
var
  hc,vc: integer;
begin
  case side of
    dbsTopLeft: exit(point(x,y));
    dbsTop:
    begin
      hc:=width div 2;
      if (abs(position)>hc) then
      begin
        if position>0 then position:=hc else position:=-hc;
      end;

      exit(point(x+hc+position,y));
    end;

    dbsTopRight: exit(point(x+width,y));

    dbsRight:
    begin
      vc:=height div 2;
      if (abs(position)>vc) then
      begin
        if position>0 then position:=vc else position:=-vc;
      end;
      exit(point(x+width,y+vc+position));
    end;

    dbsBottomRight: exit(point(x+width,y+height));

    dbsBottom:
    begin
      hc:=width div 2;
      if (abs(position)>hc) then
      begin
        if position>0 then position:=hc else position:=-hc;
      end;

      exit(point(x+hc+position,y+height));
    end;

    dbsBottomLeft: exit(point(x,y+height));

    dbsLeft:
    begin
      vc:=height div 2;
      if (abs(position)>vc) then
      begin
        if position>0 then position:=vc else position:=-vc;
      end;
      exit(point(x,y+vc+position));
    end;
  end;
end;

procedure TDiagramBlock.resize(xpos, ypos: integer; side: TDiagramBlockSide);
var d: integer;
  procedure resizeLeft;
  begin
    if xpos>=(x+width-1) then xpos:=x+width-1;
    d:=xpos-x;
    width:=width-d;
    x:=xpos;
  end;

  procedure resizeTop;
  begin
    if ypos>=(y+height-captionheight-1) then ypos:=y+height-captionheight-1;
    d:=ypos-y;
    height:=height-d;
    y:=ypos;
  end;

  procedure resizeRight;
  begin
    width:=xpos-x;
    if width<1 then width:=1;
  end;

  procedure resizeBottom;
  begin
    height:=ypos-y;
    if height<captionheight then height:=captionheight;
  end;

begin
  case side of
    dbsTopLeft:
    begin
      resizeLeft;
      resizeTop;
    end;

    dbsTop: resizeTop;
    dbsTopRight:
    begin
      resizeTop;
      resizeRight;
    end;

    dbsRight: resizeRight;
    dbsBottomRight:
    begin
      resizeBottom;
      resizeRight;
    end;

    dbsBottom: resizeBottom;
    dbsBottomLeft:
    begin
      resizeBottom;
      resizeLeft;
    end;

    dbsLeft: resizeLeft;
  end;

end;

constructor TDiagramBlock.create(graphConfig: TDiagramConfig);
begin
  data:=tstringlist.create;
  config:=GraphConfig;
  x:=0;
  y:=0;
  width:=100;
  height:=100;

end;

destructor TDiagramBlock.destroy;
begin
  if assigned(OnDestroy) then
    OnDestroy(self);

  //owner.NotifyBlockDestroy(self);

  data.free;

  inherited destroy;
end;



end.

