unit diagramlink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, DiagramTypes, Diagramblock;

type

  TDiagramLink=class
  private
    config: TDiagramConfig;
    origin: TDiagramBlockSideDescriptor;
    destination: TDiagramBlockSideDescriptor;
    points: array of tpoint; //list of coordinates where to go first before going to the toblock

    useCustomColor: boolean;
    fCustomColor: TColor;

    useCustomLineThickness: boolean;
    fCustomLineThickness: integer;

    useCustomArrowStyles: boolean;
    fCustomArrowStyles: TArrowStyles;

    fName: string;

    function getYPosFromX(x: single; linestart: tpoint; lineend: tpoint): double;
    function getXPosFromY(y: single; linestart: tpoint; lineend: tpoint): double;

    function getCenterPoint(linestart: tpoint; lineend: tpoint): tpoint;

    function ptInLine(pt: tpoint; linestart: tpoint; lineend: tpoint): boolean;
    function getLineColor: TColor;
    procedure setLineColor(c: TColor);
    function getLineThickness: integer;
    procedure setLineThickness(i: integer);
    function getArrowStyles: TArrowStyles;
    procedure setArrowStyles(s: TArrowStyles);

    function getCenterAdjustForBorderSide(side: TDiagramBlockSide): TPoint;
    function getAngle(originpoint: TPoint; destinationpoint: TPoint): single;
    procedure DrawArrow(originpoint: TPoint; rot: single; centeradjust: tpoint);

    procedure drawArrowInCenter;
  public
    procedure render;

    procedure createPoint(p: tpoint; insertAt: integer=-1);
    function isOverLine(x,y: integer): boolean;
    function isAtAttachPoint(x,y: integer; out bsd: TDiagramBlockSideDescriptor): boolean;
    function getPointIndexAt(x,y: integer): integer;
    function getPoint(index: integer): TPoint;
    function getPointCount: integer;

    function hasLinkToBlock(b: TDiagramBlock):boolean;
    procedure updatePointPosition(index: integer; newpos: TPoint);

    procedure updateSide(descriptor: TDiagramBlockSideDescriptor);

    procedure RemoveAllPlotPoints;
    procedure ResetToDefault;

    function getOriginDescriptor: TDiagramBlockSideDescriptor;
    function getDestinationDescriptor: TDiagramBlockSideDescriptor;

    procedure setOriginDescriptor(d: TDiagramBlockSideDescriptor);
    procedure setDestinationDescriptor(d: TDiagramBlockSideDescriptor);


    property PlotPoints[index: integer]: TPoint read getPoint write updatePointPosition;
    constructor create(diagramconfig: TDiagramConfig; _origin,_destination: TDiagramBlockSideDescriptor);
    destructor destroy; override;
  published
    property OriginBlock: TDiagramBlock read origin.Block write origin.block;
    property DestinationBlock: TDiagramBlock read destination.Block write destination.block;
    property PointCount: integer read getPointCount;
    property LineColor: TColor read getLineColor write setLineColor;
    property LineThickness: integer read getLineThickness write setLineThickness;
    property ArrowStyles: TArrowStyles read getArrowStyles write setArrowStyles;
    property Name: string read fName write fName;
  end;

implementation

uses math, types;

function TDiagramLink.getLineThickness: integer;
begin
  if useCustomLineThickness then
    result:=fCustomLineThickness
  else
    result:=config.LineThickness;
end;

procedure TDiagramLink.setLineThickness(i: integer);
begin
  fCustomLineThickness:=i;
  useCustomLineThickness:=true;
end;


function TDiagramLink.getLineColor: TColor;
begin
  if useCustomColor then
    result:=fCustomColor
  else
    result:=config.LineColor;
end;

procedure TDiagramLink.setLineColor(c: TColor);
begin
  fCustomColor:=c;
  useCustomColor:=true;
end;

function TDiagramLink.getArrowStyles: TArrowStyles;
begin
  if useCustomArrowStyles then
    result:=fCustomArrowStyles
  else
    result:=config.arrowStyles;
end;

procedure TDiagramLink.setArrowStyles(s: TArrowStyles);
begin
  fCustomArrowStyles:=s;
  useCustomArrowStyles:=true;
end;



function TDiagramLink.getYPosFromX(x: single; linestart: tpoint; lineend: tpoint): double;
var
  deltax,deltay: integer;
  slope: double;
begin
  deltax:=lineend.x-linestart.x;
  deltay:=lineend.y-linestart.y;

  if deltax=0 then
    result:=infinity
  else
  begin
    slope:=deltay/deltax;
    result:=slope*double(x-linestart.x)+double(linestart.y);


  end;
end;

function TDiagramLink.getXPosFromY(y: single; linestart: tpoint; lineend: tpoint): double;
var
  deltax,deltay: integer;
  slope: double;
begin
  deltax:=lineend.x-linestart.x;
  deltay:=lineend.y-linestart.y;

  if deltay=0 then
    result:=Infinity
  else
    slope:=deltax/deltay;

  result:=slope*double(y-linestart.y)+double(linestart.x);

  // config.canvas.Pixels[trunc(result),y]:=$00ff00;
end;

function TDiagramLink.getCenterPoint(linestart: tpoint; lineend: tpoint): tpoint;
//calculate the center between two lines and return the x,y position
var x1,x2: integer;
  _x: single;
begin
  x1:=min(linestart.x,lineend.x);
  x2:=max(linestart.x,lineend.x);

  _x:=single(x1)+(single(x2-x1) / 2);
  result.x:=trunc(_x);

  if x2-x1=0 then
    result.y:=linestart.y+((max(linestart.y,lineend.y)-min(linestart.y,lineend.y)) div 2)
  else
    result.y:=trunc(getYPosFromX(_x,linestart,lineend));
end;

function TDiagramLink.ptInLine(pt: tpoint; linestart: tpoint; lineend: tpoint): boolean;
var
  r: trect;
  a: double;

  y,y2: double;
  t: double;
begin
  result:=false;

  r:=trect.Create(linestart,lineend,true);

  a:=config.LineThickness*1.2;
  if a=0 then a:=1;

  if inrange(pt.x,r.Left-a,r.Right+a) and (inrange(pt.y,r.top-a,r.bottom+a)) then
  begin
    y:=getYPosFromX(floor(pt.x-a),linestart,lineend);
    y2:=getYPosFromX(ceil(pt.x+a),linestart,lineend);

    getXPosFromY(pt.y,linestart,lineend);

    if y>y2 then
    begin
      t:=y;
      y:=y2;
      y2:=t;
    end;

    if (y=Infinity) or (y2=Infinity) then exit(true);

    result:=inrange(double(pt.y),y-a,y2+a);
  end;

end;

const arrow:array [0..2] of TPoint =(
  (x:-5;y:-5),
  (x:5;y:0),
  (x:-5;y:5)
);

function TDiagramLink.getCenterAdjustForBorderSide(side: TDiagramBlockSide): TPoint;
begin
  case side of
    dbsTop: exit(point(0,-5));
    dbsTopRight: exit(point(5,-5));
    dbsRight: exit(point(5,0));
    dbsBottomRight: exit(point(5,5));
    dbsBottom: exit(point(0,5));
    dbsBottomLeft: exit(point(-5,5));
    dbsLeft: exit(point(-5,0));
    dbsTopLeft: exit(point(-5,-5));
    else exit(point(0,0)); //never
  end;
end;

function TDiagramLink.getAngle(originpoint: TPoint; destinationpoint: TPoint): single;
var dx,dy: integer;
begin
  dx:=destinationpoint.x-originpoint.x;
  dy:=destinationpoint.y-originpoint.y;

  result:=arctan2(dy,dx);
end;

procedure TDiagramLink.DrawArrow(originpoint: TPoint; rot: single; centerAdjust: TPoint);
var
  c: tcanvas;


 // rot: float;

  r: float;
  p: float;

  arr: array [0..2] of tpoint;

 // oldp,oldb: tcolor;
  i: integer;
begin
  //calculate the angle to point at based from original to destination
  //originpoint.
  c:=config.canvas;
  arr:=arrow;

  for i:=0 to 2 do
  begin
    r:=sqrt(sqr(arr[i].X+centerAdjust.x) + sqr(arr[i].Y+centerAdjust.y));
    p := rot + arcTan2(arr[i].Y+centerAdjust.y , arr[i].X+centerAdjust.x);
    arr[i].X := Round(r * cos(p))+originpoint.x;
    arr[i].Y := Round(r * sin(p))+originpoint.y;
  end;

  c.Polygon(arr);
end;

procedure TDiagramLink.drawArrowInCenter;
{
get the full distance of the all lines between origin and destination
get the half of this

find out which line this belongs to
then calculate the x position where this is in the line
calculate the y coordinate, and draw the arrow there
}
var
  linelist: array of record
    p1,p2: tpoint;
  end;
  originpoint: TPoint;
  destinationpoint: tpoint;

  totallength: single;
  halflength: single;
  temp: single;

  distancetogoback: single;

  deltax,deltay: single;
  angle: single;

  xpos,ypos: single;
  i: integer;
begin
  originpoint:=origin.block.getConnectPosition(origin.side, origin.sideposition);
  destinationpoint:=destination.block.getConnectPosition(destination.side,destination.sideposition);

  setlength(linelist,1+length(points));
  linelist[0].p1:=originpoint;

  for i:=0 to length(points)-1 do
  begin
    linelist[i].p2:=points[i];
    linelist[i+1].p1:=points[i];
  end;

  linelist[length(linelist)-1].p2:=destinationpoint;

  totallength:=0;
  for i:=0 to length(linelist)-1 do
    totallength:=totallength+linelist[i].p1.Distance(linelist[i].p2);

  temp:=0;
  halflength:=totallength / 2;
  for i:=0 to length(linelist)-1 do
  begin
    temp:=temp+linelist[i].p1.Distance(linelist[i].p2);
    if temp>halflength then //found it
    begin
      distancetogoback:=temp-halflength;

      deltax:=linelist[i].p1.x-linelist[i].p2.x;
      deltay:=linelist[i].p1.y-linelist[i].p2.y;

      if deltax=0 then exit;
      if deltay=0 then exit;

      angle:=arctan(deltay/deltax); //works

      if deltax<0 then
        xpos:=cos(angle)*distancetogoback
      else
        xpos:=cos(angle)*-distancetogoback;


      ypos:=getYPosFromX(linelist[i].p2.x-xpos,linelist[i].p1, linelist[i].p2);
      if ypos=Infinity then ypos:=linelist[i].p2.y;

      drawArrow(point(linelist[i].p2.x-trunc(xpos),trunc(ypos)),getAngle(linelist[i].p1,linelist[i].p2),point(0,0));

      exit;

    end;
  end;



end;

procedure TDiagramLink.render;
var
  oldw: integer;
  oldc: tcolor;
  oldbc: tcolor;
  i: integer;
  c: TCanvas;

  originpoint: tpoint;
  destinationpoint: tpoint;

  directionpoint: TPoint;


  p1,p2: tpoint;

begin
  c:=config.canvas;



  oldw:=c.pen.Width;
  oldc:=c.pen.color;

  c.pen.width:=config.LineThickness;
  c.pen.color:=LineColor;

  originpoint:=origin.block.getConnectPosition(origin.side, origin.sideposition);
  destinationpoint:=destination.block.getConnectPosition(destination.side,destination.sideposition);

  c.PenPos:=originpoint;

  for i:=0 to Length(points)-1 do
  begin
    c.LineTo(points[i]);

    if config.DrawPlotPoints then
    begin
      oldbc:=c.Brush.Color;
      c.Brush.Color:=clred;
      c.FillRect(points[i].x-config.PlotPointSize,points[i].y-config.PlotPointSize,points[i].x+config.PlotPointSize,points[i].y+config.PlotPointSize);
      c.brush.color:=oldbc;
    end;
  end;

  c.LineTo(destinationpoint);


  if asOrigin in arrowStyles then
  begin
    if Length(points)>0 then
      directionpoint:=points[0]
    else
      directionpoint:=destinationpoint;

    drawArrow(originpoint, getAngle(originpoint,directionpoint),getCenterAdjustForBorderSide(origin.side));
  end;

  if asDestination in ArrowStyles then
  begin
    if Length(points)>0 then
      directionpoint:=points[length(points)-1]
    else
      directionpoint:=originpoint;

    drawArrow(destinationpoint, getAngle(directionpoint,destinationpoint),getCenterAdjustForBorderSide(destination.side));
  end;

  if asPoints in arrowstyles then
  begin
    for i:=0 to length(points)-1 do
    begin
      if i<length(points)-1 then
        directionpoint:=points[i+1]
      else
        directionpoint:=destinationpoint;

      drawArrow(points[i], getAngle(points[i],directionpoint),point(0,0));
    end;
  end;

  if asCenterBetweenPoints in arrowstyles then
  begin
    if length(points)=0 then
    begin
      p1:=originpoint;
      p2:=destinationpoint;
      drawArrow(getCenterPoint(p1,p2), getAngle(p1,p2),point(0,0));
    end;

    for i:=0 to length(points)-1 do
    begin
      if i=0 then
      begin
        p1:=originpoint;
        p2:=points[i];

        drawArrow(getCenterPoint(p1,p2), getAngle(p1,p2),point(0,0));
      end
      else
      if i=length(points)-1 then
      begin
        p1:=points[i];
        p2:=destinationpoint;

        drawArrow(getCenterPoint(p1,p2), getAngle(p1,p2),point(0,0));
      end else
      begin
        p1:=points[i-1];
        p2:=points[i];
        drawArrow(getCenterPoint(p1,p2), getAngle(p1,p2),point(0,0));
      end;
      drawArrow(points[i], getAngle(points[i],directionpoint),point(0,0));
    end;
  end;

  if asCenter in arrowstyles then
  begin
    //this is more an excercise in geometry for me than anything useful but hey...
    drawArrowInCenter;
  end;


  c.pen.Width:=oldw;
  c.pen.color:=oldc;
end;

function TDiagramLink.isOverLine(x,y: integer): boolean;
var
  i: integer;
  start: tpoint;
  dest: tpoint;
begin
  start:=origin.block.getConnectPosition(origin.side, origin.sideposition);
  for i:=0 to length(points)-1 do
  begin
    dest:=points[i];
    if ptInLine(point(x,y),start,dest) then exit(true);
    start:=dest;
  end;

  //still here
  dest:=destination.block.getConnectPosition(destination.side, destination.sideposition);
  result:=ptInLine(point(x,y),start,dest);
end;

function TDiagramLink.isAtAttachPoint(x,y: integer; out bsd: TDiagramBlockSideDescriptor): boolean;
var
  ap: Tpoint;
begin
  ap:=origin.block.getConnectPosition(origin.side,origin.sideposition);
  if ap.Distance(point(x,y))<=config.PlotPointSize then
  begin
    bsd:=origin;
    exit(true);
  end;

  ap:=destination.block.getConnectPosition(destination.side,destination.sideposition);
  if ap.Distance(point(x,y))<=config.PlotPointSize then
  begin
    bsd:=destination;
    exit(true);
  end;

  exit(false);
end;

function TDiagramLink.getPointIndexAt(x,y: integer): integer;
var
  p: tpoint;
  i: integer;
begin
  p.x:=x;
  p.y:=y;

  result:=-1;
  for i:=0 to length(points)-1 do
  begin
    if p.distance(points[i])<=config.PlotPointSize then
      exit(i);
  end;
end;



procedure TDiagramLink.createPoint(p: tpoint; insertAt: integer=-1);
var
  i: integer;
  start: tpoint;
  dest: tpoint;
  index: integer;
begin
  if insertat=-1 then
  begin
    //figure out where in the line this is, or if it's already a point
    for i:=0 to length(points)-1 do
      if points[i].Distance(p)<(config.LineThickness*2) then exit; //already a point here



    //no point here yet, find where in the line this is (assuming it is)
    index:=-1;
    start:=origin.block.getConnectPosition(origin.side, origin.sideposition);
    for i:=0 to length(points)-1 do
    begin
      dest:=points[i];
      if ptInLine(p,start,dest) then
      begin
        index:=i;
        break;
      end;
      start:=dest;
    end;

    if index=-1 then
    begin
      dest:=destination.block.getConnectPosition(destination.side, destination.sideposition);
      if ptInLine(p,start,dest) then
        index:=length(points);
    end;

    if index=-1 then exit; //something went wrong


  end
  else
  begin
    index:=insertAt;
    if index>length(points) then
      index:=length(points);
  end;

  //add it after the current index
  setlength(points, length(points)+1);
  for i:=length(points)-1 downto index+1 do
    points[i]:=points[i-1];

  points[index]:=p;
end;

procedure TDiagramLink.RemoveAllPlotPoints;
begin
  setlength(points,0);
end;

function TDiagramLink.getPointCount: integer;
begin
  result:=length(points);
end;

procedure TDiagramLink.updatePointPosition(index: integer; newpos: TPoint);
begin
  if (index<0) or (index>=length(points)) then exit;

  points[index]:=newpos;
end;

function TDiagramLink.getPoint(index: integer): tpoint;
begin
  if (index<0) or (index>=length(points)) then raise exception.create('Index out of bounds');

  result:=points[index];
end;

procedure TDiagramLink.updateSide(descriptor: TDiagramBlockSideDescriptor);
begin
  if descriptor.block=origin.block then
  begin
    origin.side:=descriptor.side;
    origin.sideposition:=descriptor.sideposition;
  end
  else if descriptor.block=destination.block then
  begin
    destination.side:=descriptor.side;
    destination.sideposition:=descriptor.sideposition;
  end;
end;

procedure TDiagramLink.ResetToDefault;
begin
  useCustomColor:=false;
  useCustomLineThickness:=false;
  useCustomArrowStyles:=false;
end;

function TDiagramLink.hasLinkToBlock(b: TDiagramBlock):boolean;
begin
  result:=(b=origin.block) or (b=destination.block);
end;

function TDiagramLink.getOriginDescriptor: TDiagramBlockSideDescriptor;
begin
  result:=origin;
end;

procedure TDiagramLink.setOriginDescriptor(d: TDiagramBlockSideDescriptor);
begin
  origin:=d;
end;

function TDiagramLink.getDestinationDescriptor: TDiagramBlockSideDescriptor;
begin
  result:=destination;
end;

procedure TDiagramLink.setDestinationDescriptor(d: TDiagramBlockSideDescriptor);
begin
  destination:=d;
end;

constructor TDiagramLink.create(diagramconfig: TDiagramConfig; _origin,_destination: TDiagramBlockSideDescriptor);
begin
  config:=diagramconfig;

  origin:=_origin;
  destination:=_destination;
end;

destructor TDiagramLink.destroy;
begin
  inherited destroy;
end;

end.

