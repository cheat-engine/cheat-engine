unit diagramlink;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, graphics, DiagramTypes, Diagramblock, gl, glu, GLext;

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

    useCustomArrowSize: boolean;
    fCustomArrowSize: integer;

    fName: string;

    fmaxx: integer; //set after render
    fmaxy: integer;

    fOnDblClick: TNotifyEvent;
    fOnDestroy: TNotifyEvent;
    ftag: qword;



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

    function getArrowSize: integer;
    procedure setArrowSize(i: integer);


    function getAngle(originpoint: TPoint; destinationpoint: TPoint): single;
    procedure DrawPlotPoint(point: TPoint);
    procedure DrawArrow(originpoint: TPoint; rot: single; centeradjust: tpoint);

    procedure drawArrowInCenter;
    function zoompoint(p: tpoint): TPoint;
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

    procedure saveToStream(f: tstream);
    procedure loadFromStream(f: tstream; blocks: tlist);


    property PlotPoints[index: integer]: TPoint read getPoint write updatePointPosition;
    property OnDestroy: TNotifyEvent read fOnDestroy write fOnDestroy;
    constructor create(diagramconfig: TDiagramConfig; _origin,_destination: TDiagramBlockSideDescriptor);
    constructor createFromStream(diagramconfig: TDiagramConfig; f: tstream; blocks: TList);
    destructor destroy; override;
  published
    property OriginBlock: TDiagramBlock read origin.Block write origin.block;
    property DestinationBlock: TDiagramBlock read destination.Block write destination.block;
    property PointCount: integer read getPointCount;
    property LineColor: TColor read getLineColor write setLineColor;
    property LineThickness: integer read getLineThickness write setLineThickness;
    property ArrowStyles: TArrowStyles read getArrowStyles write setArrowStyles;
    property ArrowSize: integer read getArrowSize write setArrowSize;
    property Name: string read fName write fName;
    property maxx: integer read fmaxx;
    property maxy: integer read fmaxy;
    property OnDblClick: TNotifyEvent read fOnDblClick write fOnDblClick;
    property Tag: qword read ftag write ftag;
  end;

implementation

uses math, types, typinfo;

var
  PlotPointVertices:array [0..3] of array [0..1] of single;
  PlotPointIndices: array [0..3] of WORD;


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

function TDiagramLink.getArrowSize: integer;
begin
  if useCustomArrowStyles then
    result:=fCustomArrowSize
  else
    result:=config.arrowSize;
end;

procedure TDiagramLink.setArrowSize(i: integer);
begin
  fCustomArrowSize:=i;
  useCustomArrowSize:=true;
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
  begin
    slope:=deltax/deltay;
    result:=slope*double(y-linestart.y)+double(linestart.x);
  end;



  // config.canvas.Pixels[trunc(result),y]:=$00ff00;
end;

function TDiagramLink.getCenterPoint(linestart: tpoint; lineend: tpoint): tpoint;
//calculate the center between two points and return the x,y position
var x1,x2,y1,y2: integer;
  _x: single;
  _y: single;
begin
  x1:=min(linestart.x,lineend.x);
  x2:=max(linestart.x,lineend.x);

  _x:=single(x1)+(single(x2-x1) / 2);
  result.x:=trunc(_x);

  y1:=min(linestart.y,lineend.y);
  y2:=max(linestart.y,lineend.y);

  _y:=single(y1)+(single(y2-y1) / 2);
  result.y:=trunc(_y);
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


function TDiagramLink.getAngle(originpoint: TPoint; destinationpoint: TPoint): single;
var dx,dy: integer;
begin
  dx:=destinationpoint.x-originpoint.x;
  dy:=destinationpoint.y-originpoint.y;

  result:=arctan2(dy,dx);
end;




procedure TDiagramLink.DrawPlotPoint(point: TPoint);
var
  c: TCanvas;
  oldbc: TColor;
  pps: single;
begin
  if config.UseOpenGL then
  begin
    glColor3ub(255,0,0);

    if config.CanUsebuffers then
    begin
      if config.plotpointvertexbuf=0 then
      begin
        glGenBuffers(1,@config.plotpointvertexbuf);
        glBindBuffer(GL_ARRAY_BUFFER, config.plotpointvertexbuf);

        pps:=(config.PlotPointSize / 2);
        PlotPointVertices[0][0]:=-pps;
        PlotPointVertices[0][1]:=-pps;

        PlotPointVertices[1][0]:=+pps;
        PlotPointVertices[1][1]:=-pps;

        PlotPointVertices[2][0]:=+pps;
        PlotPointVertices[2][1]:=+pps;

        PlotPointVertices[3][0]:=-pps;
        PlotPointVertices[3][1]:=+pps;
        glBufferData(GL_ARRAY_BUFFER,4*2*sizeof(single), @PlotPointVertices[0], GL_STATIC_DRAW);

        glGenBuffers(1,@config.plotpointindexbuf);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, config.plotpointindexbuf);
        PlotPointIndices[0]:=0;
        PlotPointIndices[1]:=1;
        PlotPointIndices[2]:=2;
        PlotPointIndices[3]:=3;
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(WORD)*4, @PlotPointIndices[0], GL_STATIC_DRAW);
      end;

      glBindBuffer(GL_ARRAY_BUFFER, config.plotpointvertexbuf);
      glEnableClientState ( GL_VERTEX_ARRAY );
      glVertexPointer(2,GL_FLOAT,0,nil);

      glTranslatef(point.x,point.y,0);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, config.plotpointindexbuf);
      glDrawRangeElements(GL_QUADS,0,3,4,GL_UNSIGNED_SHORT,nil);
      glLoadIdentity;
    end
    else
    begin
      pps:=(config.PlotPointSize / 2);
      glBegin(GL_QUADS);
      glVertex2f(point.x-pps, point.y-pps);
      glVertex2f(point.x+pps, point.y-pps);
      glVertex2f(point.x+pps, point.y+pps);
      glVertex2f(point.x-pps, point.y+pps);
      glEnd;
    end;


  end
  else
  begin
    c:=config.canvas;
    oldbc:=c.Brush.Color;
    c.Brush.Color:=clred;
    c.FillRect(point.x-trunc((config.PlotPointSize/2)*config.zoom),point.y-trunc((config.PlotPointSize/2)*config.zoom),point.x+ceil((config.PlotPointSize/2)*config.zoom),point.y+ceil((config.PlotPointSize/2)*config.zoom));
    c.brush.color:=oldbc;
  end;
end;

const arrow:array [0..2] of TPoint =(
  (x:-5;y:-5),
  (x:5;y:0),
  (x:-5;y:5)
);

procedure TDiagramLink.DrawArrow(originpoint: TPoint; rot: single; centerAdjust: TPoint);
var
  c: tcanvas;
 // rot: float;

  r: float;
  p: float;

  arr: array [0..2] of tpoint;

 // oldp,oldb: tcolor;
  i: integer;

  _r,_g,_b: byte;

  sizescale: single;
begin
  //calculate the angle to point at based from original to destination
  //originpoint.

  sizescale:=1;

  if ArrowSize<>5 then
    sizescale:=arrowsize/5;

  if config.UseOpenGL then
  begin
    //glTranslatef(centeradjust.x,centeradjust.y,0);

    glLoadIdentity;


    glTranslatef(originpoint.x,originpoint.y,0);
    glRotatef(radtodeg(rot),0,0,1);
    glTranslatef(centeradjust.x,centeradjust.y,0);
    glScalef(config.zoom,config.zoom,1);

    RedGreenBlue(linecolor,_r,_g,_b);

    glColor3f(_r/255,_g/255,_b/255);
    glBegin(GL_TRIANGLES);
    glVertex2f(arrow[0].x, arrow[0].y);
    glVertex2f(arrow[1].x, arrow[1].y);
    glVertex2f(arrow[2].x, arrow[2].y);
    //glVertex2f(arrow[0].x, arrow[0].y);
    glEnd;

    //glcolor3d(1,0,0);


    glBegin(GL_LINE_STRIP);
    glVertex2f(arrow[0].x, arrow[0].y);
    glVertex2f(arrow[1].x, arrow[1].y);
    glVertex2f(arrow[2].x, arrow[2].y);
    glVertex2f(arrow[0].x, arrow[0].y);
    glEnd;
    glLoadIdentity;
  end
  else
  begin
    c:=config.canvas;
    arr:=arrow;


    for i:=0 to 2 do
    begin
      if sizescale<>1 then
      begin
        arr[i].x:=ceil(arr[i].x*sizescale);
        arr[i].y:=ceil(arr[i].y*sizescale);
      end;

      r:=sqrt(sqr(arr[i].X+centerAdjust.x) + sqr(arr[i].Y+centerAdjust.y));
      p := rot + arcTan2(arr[i].Y+centerAdjust.y , arr[i].X+centerAdjust.x);
      arr[i].X := Round(r * cos(p)*config.zoom)+originpoint.x;
      arr[i].Y := Round(r * sin(p)*config.zoom)+originpoint.y;
    end;

    c.Polygon(arr);
  end;
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

  adjust: Tpoint;
begin
  adjust.x:=config.scrollx;
  adjust.y:=config.scrolly;

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

      drawArrow(zoompoint(point(linelist[i].p2.x-trunc(xpos),trunc(ypos))-adjust),getAngle(zoompoint(linelist[i].p1-adjust),zoompoint(linelist[i].p2-adjust)),point(0,0));

      exit;

    end;
  end;
end;

function TDiagramLink.zoompoint(p: tpoint): TPoint;
begin
  result:=point(ceil(p.x*config.zoom),ceil(p.y*config.zoom));
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

  mx,my: integer;

  adjust: Tpoint;
  r,g,b: byte;
  t: TColorref;
begin
  adjust.x:=config.scrollx;
  adjust.y:=config.scrolly;

  if config.UseOpenGL then
  begin
    RedGreenBlue(linecolor,r,g,b);
    glColor3ub(r,g,b);
    glLineWidth(LineThickness*config.zoom);
    oldw:=0;
    oldc:=0;
    c:=nil;
  end
  else
  begin
    c:=config.canvas;

    oldw:=c.pen.Width;
    oldc:=c.pen.color;

    c.pen.width:=ceil(LineThickness*config.zoom);
    c.pen.color:=LineColor;
  end;


  originpoint:=origin.block.getConnectPosition(origin.side, origin.sideposition);
  destinationpoint:=destination.block.getConnectPosition(destination.side,destination.sideposition);

  mx:=originpoint.x;
  my:=originpoint.y;

  mx:=max(mx, destinationpoint.x);
  my:=max(my, destinationpoint.y);

  if config.UseOpenGL then
  begin
    glBegin(GL_LINE_STRIP);
    glVertex2f((originpoint.x-config.scrollx)*config.zoom, (originpoint.y-config.scrolly)*config.zoom);
  end
  else
  begin
    c.PenPos:=zoompoint(originpoint-adjust);
  end;

  for i:=0 to Length(points)-1 do
  begin
    mx:=max(mx, points[i].x);
    my:=max(my, points[i].y);

    if config.UseOpenGL then
      glVertex2f((points[i].x-config.scrollx)*config.zoom, (points[i].y-config.scrolly)*config.zoom)
    else
      c.LineTo(zoompoint(points[i]-adjust));
  end;

  fmaxx:=mx;
  fmaxy:=my;

  if config.UseOpenGL then
  begin
    glVertex2f((destinationpoint.x-config.scrollx)*config.zoom, (destinationpoint.y-config.scrolly)*config.zoom);

    glEnd;
  end
  else
    c.LineTo(zoompoint(destinationpoint-adjust));

  if config.DrawPlotPoints then
    for i:=0 to length(points)-1 do
      drawPlotPoint(zoompoint(points[i]-adjust));

  if asOrigin in arrowStyles then
  begin
    if Length(points)>0 then
      directionpoint:=points[0]
    else
      directionpoint:=destinationpoint;

    drawArrow(zoompoint(originpoint-adjust), getAngle(zoompoint(originpoint-adjust),zoompoint(directionpoint-adjust)),point(5,0));
  end;

  if asDestination in ArrowStyles then
  begin
    if Length(points)>0 then
      directionpoint:=points[length(points)-1]
    else
      directionpoint:=originpoint;

    drawArrow(zoompoint(destinationpoint-adjust), getAngle(zoompoint(directionpoint-adjust),zoompoint(destinationpoint-adjust)),point(-5,0)); //getCenterAdjustForBorderSide(destination.side));
  end;

  if asPoints in arrowstyles then
  begin
    for i:=0 to length(points)-1 do
    begin
      if i<length(points)-1 then
        directionpoint:=points[i+1]
      else
        directionpoint:=destinationpoint;

      drawArrow(zoompoint(points[i]-adjust), getAngle(zoompoint(points[i]-adjust),zoompoint(directionpoint-adjust)),point(0,0));
    end;
  end;

  if asCenterBetweenPoints in arrowstyles then
  begin
    p1:=zoompoint(originpoint-adjust);


    for i:=0 to length(points)-1 do
    begin
      p2:=zoompoint(points[i]-adjust);
      drawArrow(getCenterPoint(p1,p2), getAngle(p1,p2),point(0,0));

      p1:=p2;
    end;
    p2:=zoompoint(destinationpoint-adjust);
    drawArrow(getCenterPoint(p1,p2), getAngle(p1,p2),point(0,0));


  end;

  if asCenter in arrowstyles then
  begin
    //this is more an excercise in geometry for me than anything useful but hey...
    drawArrowInCenter;
  end;


  if not config.UseOpenGL then
  begin
    c.pen.Width:=oldw;
    c.pen.color:=oldc;
  end;
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

procedure TDiagramLink.saveToStream(f: tstream);
var i: integer;
begin
  f.WriteAnsiString('LNK');

  f.WriteAnsiString(fname);

  f.WriteDWord(origin.block.BlockID);
  f.WriteByte(integer(origin.side));
  f.WriteDWord(integer(origin.sideposition));

  f.WriteDWord(destination.block.BlockID);
  f.WriteByte(integer(destination.side));
  f.WriteDWord(integer(destination.sideposition));

  f.WriteDWord(length(points));
  for i:=0 to length(points)-1 do
  begin
    f.writeDword(points[i].x);
    f.writeDword(points[i].y);
  end;

  f.writebyte(ifthen(useCustomColor,1,0));
  if useCustomColor then f.WriteDWord(fCustomColor);

  f.writebyte(ifthen(useCustomLineThickness,1,0));
  if useCustomLineThickness then f.WriteDWord(fCustomLineThickness);

  f.writebyte(ifthen(useCustomArrowSize,1,0));
  if useCustomArrowSize then f.WriteDWord(fCustomArrowSize);

  f.writebyte(ifthen(useCustomArrowStyles,1,0));
  if useCustomArrowStyles then f.WriteByte(byte(fCustomArrowStyles));

end;

procedure TDiagramLink.loadFromStream(f: tstream; blocks: tlist);
  function idToBlock(id: integer): TDiagramBlock;
  var
    i: integer;
    b: TDiagramBlock;
  begin
    result:=nil;

    if (id<blocks.count) and (TDiagramBlock(blocks[id]).BlockID=id) then exit(TDiagramBlock(blocks[id]));

    //unexpected blockorder

    for i:=0 to blocks.count-1 do
    begin
      b:=TDiagramBlock(blocks[i]);
      if b.blockid=id then
        exit(b);
    end;
  end;

var  i: integer;
begin
  if f.ReadAnsiString<>'LNK' then raise exception.create('Link read error');

  fname:=f.ReadAnsiString;

  origin.block:=idToBlock(f.ReadDWord);
  if origin.block=nil then
    raise exception.create('Link read error. Origin for a link not found');

  origin.side:=TDiagramBlockSide(f.ReadByte);
  origin.sideposition:=f.ReadDWord;

  destination.block:=idToBlock(f.ReadDWord);
  if destination.block=nil then
    raise exception.create('Link read error. Destination for a link not found');
  destination.side:=TDiagramBlockSide(f.ReadByte);
  destination.sideposition:=f.ReadDWord;

  setlength(points, f.ReadDWord);
  for i:=0 to length(points)-1 do
  begin
    points[i].x:=f.ReadDWord;
    points[i].y:=f.ReadDWord;
  end;

  useCustomColor:=f.readbyte=1;
  if useCustomColor then fCustomColor:=f.ReadDWord;

  useCustomLineThickness:=f.readbyte=1;
  if useCustomLineThickness then fCustomLineThickness:=f.ReadDWord;

  useCustomArrowSize:=f.readbyte=1;
  if useCustomArrowSize then fCustomArrowSize:=f.ReadDWord;

  useCustomArrowStyles:=f.readbyte=1;
  if useCustomArrowStyles then fCustomArrowStyles:=TArrowStyles(f.ReadByte);


end;

constructor TDiagramLink.create(diagramconfig: TDiagramConfig; _origin,_destination: TDiagramBlockSideDescriptor);
begin
  config:=diagramconfig;

  origin:=_origin;
  destination:=_destination;
end;

constructor TDiagramLink.createFromStream(diagramconfig: TDiagramConfig; f: tstream; blocks: TList);
begin
  config:=diagramconfig;
  loadFromStream(f,blocks);
end;

destructor TDiagramLink.destroy;
begin
  if assigned(fOnDestroy) then
    fOnDestroy(self);

  inherited destroy;
end;

end.

