unit diagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls,Types, graphics, diagramblock, diagramlink, diagramtypes,
  LMessages;

type
  TDiagram=class(TCustomControl)
  private
    blocks: TList;
    links: TList;
    draggedblock: record
      block: TDiagramBlock;
      point: TPoint;
    end;

    draggedPoint: record
      link: TDiagramLink;
      pointindex: integer;
    end;

    draggedSideAttachPoint: record
      block: TDiagramBlock;
      link: TDiagramLink;
    end;

    resizing: record
      block: TDiagramBlock;
      side: TDiagramBlockSide;
    end;

    fAllowUserToCreatePlotPoints: boolean;
    fAllowUserToMovePlotPoints: boolean;
    fAllowUserToResizeBlocks: boolean;
    fAllowUserToMoveBlocks: boolean;
    fAllowUserToChangeAttachPoints: boolean;

    diagramConfig: TDiagramConfig;
    procedure NotifyBlockDestroy(sender: TObject);
    procedure updateBlockDragPosition(xpos,ypos: integer);
    procedure updateResizePosition(xpos,ypos: integer);
    procedure updatePointDragPosition(xpos,ypos: integer);
    procedure updateAttachPointDragPosition(xpos,ypos: integer);
    function getLineThickness: integer;
    procedure setLineThickness(t: integer);
    function getLineColor: TColor;
    procedure setLineColor(c: tcolor);
    function getDrawPlotPoints: boolean;
    procedure setDrawPlotPoints(state: boolean);
    function getPlotPointColor: TColor;
    procedure setPlotPointColor(c: tcolor);

    function getArrowStyles: TArrowStyles;
    procedure setArrowStyles(s: TArrowStyles);

    function getBlockBackground: TColor;
    procedure setBlockBackground(c: TColor);

    function getBackgroundColor: TColor;
    procedure setBackGroundColor(c: TColor);

    function getBlockCount: integer;
    function getBlock(index: integer): TDiagramBlock;
    function getLinkCount: integer;
    function getLink(index: integer): TDiagramLink;
    procedure DoAutoSideUpdate;
  protected
    procedure paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    //procedure DblClick; override;
  public
    function createBlock: TDiagramBlock;
    function addConnection(origin, destination: TDiagramBlockSideDescriptor): TDiagramLink; overload;
    function addConnection(originBlock, destinationBlock: TDiagramBlock): TDiagramLink; overload;
    procedure getConnectionsToBlock(b: TDiagramBlock; list: TList);
    procedure getConnectionsFromBlock(b: TDiagramBlock; list: TList);

    property Block[index: integer]: TDiagramBlock read getBlock;
    property Link[index: integer]:TDiagramLink read getLink;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineThickness: integer read getLineThickness write setLineThickness;
    property LineColor: Tcolor read getLineColor write setLineColor;
    property DrawPlotPoints: boolean read getDrawPlotPoints write setDrawPlotPoints;
    property PlotPointColor: TColor read getPlotPointColor write setPlotPointColor;
    property AllowUserToCreatePlotPoints: boolean read fAllowUserToCreatePlotPoints write fAllowUserToCreatePlotPoints;
    property AllowUserToMovePlotPoints: boolean read fAllowUserToMovePlotPoints write fAllowUserToMovePlotPoints;
    property AllowUserToResizeBlocks: boolean read fAllowUserToResizeBlocks write fAllowUserToResizeBlocks;
    property AllowUserToMoveBlocks: boolean read fAllowUserToMoveBlocks write fAllowUserToMoveBlocks;
    property AllowUserToChangeAttachPoints: boolean read fallowUserToChangeAttachPoints write fallowUserToChangeAttachPoints;

    property ArrowStyles: TArrowStyles read getArrowStyles write setArrowStyles;
    property BlockBackground: TColor read getBlockBackground write setBlockBackground;
    property BackGroundColor: TColor read getBackGroundColor write setBackGroundColor;

    property BlockCount: integer read getBlockCount;

    property LinkCount: integer read getLinkCount;


    property Canvas;
    property OnPaint;
    property Align;
    property Anchors;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property PopupMenu;
    property ShowHint;
    property OnShowHint;
  end;

implementation

function TDiagram.getPlotPointColor: TColor;
begin
  result:=diagramConfig.PlotPointColor;
end;

procedure TDiagram.setPlotPointColor(c: tcolor);
begin
  diagramConfig.PlotPointColor:=c;
  if parent<>nil then
    repaint;
end;

function TDiagram.getArrowStyles: TArrowStyles;
begin
  result:=diagramConfig.ArrowStyles;
end;

procedure TDiagram.setArrowStyles(s: TArrowStyles);
begin
  diagramConfig.ArrowStyles:=s;
  if parent<>nil then
    repaint;
end;

function TDiagram.getDrawPlotPoints: boolean;
begin
  result:=diagramConfig.drawPlotPoints;
end;

procedure TDiagram.setDrawPlotPoints(state: boolean);
begin
  diagramConfig.drawPlotPoints:=state;
  if parent<>nil then
    repaint;
end;

function TDiagram.getLineColor: tcolor;
begin
  result:=diagramConfig.LineColor;
end;

procedure TDiagram.setLineColor(c: tcolor);
begin
  diagramConfig.linecolor:=c;
  if parent<>nil then
    repaint;
end;

function TDiagram.getLineThickness: integer;
begin
  result:=diagramConfig.LineThickness;
end;

procedure TDiagram.setLineThickness(t: integer);
begin
  diagramConfig.LineThickness:=t;
  if parent<>nil then
    repaint;
end;

function TDiagram.getBlockBackground: TColor;
begin
  result:=diagramConfig.BlockBackground;
end;

procedure TDiagram.setBlockBackground(c: TColor);
begin
  diagramConfig.BlockBackground:=c;
  if parent<>nil then
    repaint;
end;

function TDiagram.getBackgroundColor: TColor;
begin
  result:=diagramConfig.backgroundColor;
end;

procedure TDiagram.setBackGroundColor(c: TColor);
begin
  diagramConfig.backgroundColor:=c;
  if parent<>nil then
    repaint;
end;

function TDiagram.getBlockCount: integer;
begin
  result:=blocks.Count;
end;

function TDiagram.getBlock(index: integer): TDiagramBlock;
begin
  if (index>=0) and (index<blocks.count) then
    result:=TDiagramBlock(blocks[index])
  else
    result:=nil;
end;

function TDiagram.getLinkCount: integer;
begin
  result:=links.Count;
end;

function TDiagram.getLink(index: integer): TDiagramLink;
begin
  if (index>=0) and (index<links.count) then
    result:=TDiagramLink(links[index])
  else
    result:=nil;
end;



procedure TDiagram.DoAutoSideUpdate;
var
  i,j,k: integer;
  b: TDiagramBlock;
  l: TDiagramLink;

  sides: array [0..3] of array of record
    link: TDiagramLink;
    destp: TPoint;
    slope: double;
    bsd: TDiagramBlockSideDescriptor;
  end;

  s1,s2: TDiagramBlockSideDescriptor;
  destination: TDiagramBlockSideDescriptor;

  dp: tpoint;
  op: tpoint;

  bsd: TDiagramBlockSideDescriptor;

  closestSide: TDiagramBlockSide;
  closestSideDistance: double;
  closestPoint: TPoint;

  distance: double;

  dx,dy: integer;
  inserted: boolean;

  slope: double;

  procedure insertAt(arrayindex: integer; index: integer);
  var ind: integer;
  begin
    setlength(sides[arrayindex],length(sides[arrayindex])+1);
    for ind:=length(sides[arrayindex])-1 downto index+1 do
      sides[arrayindex][ind]:=sides[arrayindex][ind-1];

    sides[arrayindex][index].destp:=closestpoint;
    sides[arrayindex][index].slope:=slope;
    sides[arrayindex][index].bsd:=bsd;
    sides[arrayindex][index].link:=l;

    inserted:=true;
  end;
begin
  for i:=0 to blocks.count-1 do
  begin
    b:=TDiagramBlock(blocks[i]);


    if b.AutoSide then
    begin
      for j:=0 to 3 do
        setlength(sides[j],0);

      for j:=0 to links.count-1 do
      begin
        l:=TDiagramLink(links[j]);

        if l.hasLinkToBlock(b) then
        begin
          destination.block:=nil;

          s1:=l.getOriginDescriptor;
          s2:=l.getDestinationDescriptor;
          if b=s1.Block then
          begin
            //origin->destination
            if l.getPointCount>0 then
              dp:=l.getPoint(0)
            else
              destination:=s2
          end
          else
          begin
            //destination->origin
            if l.getPointCount>0 then
              dp:=l.getpoint(l.getpointcount-1)
            else
              destination:=s1;
          end;

          if destination.block<>nil then
          begin
            if destination.block.autoside then
              destination:=destination.block.getClosestSideDescriptor(b.x+b.Width div 2,b.y+b.height div 2);

            dp:=destination.block.getConnectPosition(destination.side,destination.sideposition);
          end;

          if b.AutoSideDistance<0 then //just directly
          begin
            bsd:=b.getClosestSideDescriptor(dp.x,dp.y);
            l.updateSide(bsd)
          end
          else
          begin
            //center of sides only , pick the closest one
            closestside:=dbsTop;
            closestPoint:=b.getConnectPosition(dbstop);
            closestSideDistance:=closestPoint.Distance(dp);

            op:=b.getConnectPosition(dbsRight);
            distance:=op.Distance(dp);
            if distance<closestSideDistance then
            begin
              closestSide:=dbsRight;
              closestSideDistance:=distance;
              closestPoint:=op;
            end;

            op:=b.getConnectPosition(dbsBottom);
            distance:=op.Distance(dp);
            if distance<closestSideDistance then
            begin
              closestSide:=dbsBottom;
              closestSideDistance:=distance;
              closestPoint:=op;
            end;

            op:=b.getConnectPosition(dbsLeft);
            distance:=op.Distance(dp);
            if distance<closestSideDistance then
            begin
              closestSide:=dbsLeft;
              closestSideDistance:=distance;
              closestpoint:=op;
            end;

            bsd.block:=b;
            bsd.side:=closestSide;
            bsd.sideposition:=0; //set the position after checking number of lines

            l.updateSide(bsd);

            //find the best location in this array
            //based on the slope of the line
            dx:=dp.x-closestPoint.x;
            dy:=dp.y-closestpoint.y;

            inserted:=false;

            case closestSide of  //pick an array index
              dbsTop:
              begin
                //top:
                if dy=0 then
                  slope:=99999999
                else
                  slope:=(-dx)/dy;

                for k:=0 to length(sides[0])-1 do
                begin
                  if slope<sides[0][k].slope then
                  begin
                    insertAt(0,k);
                    break;
                  end;
                end;
                if not inserted then
                  insertAt(0,length(sides[0]));
              end;

              dbsRight:
              begin
                if dx=0 then
                  slope:=99999999
                else
                  slope:=dy/dx;

                for k:=0 to length(sides[1])-1 do
                begin
                  if slope<sides[1][k].slope then
                  begin
                    insertAt(1,k);
                    break;
                  end;
                end;
                if not inserted then
                  insertAt(1,length(sides[1]));
              end;

              dbsBottom:
              begin
                if dy=0 then
                  slope:=99999999
                else
                  slope:=dx/dy;

                for k:=0 to length(sides[2])-1 do
                begin
                  if slope<sides[2][k].slope then
                  begin
                    insertAt(2,k);
                    break;
                  end;
                end;
                if not inserted then
                  insertAt(2,length(sides[2]));
              end;

              dbsLeft:
              begin
                if dx=0 then
                  slope:=99999999
                else
                  slope:=(-dy)/dx;

                for k:=0 to length(sides[3])-1 do
                begin
                  if slope<sides[3][k].slope then
                  begin
                    insertAt(3,k);
                    break;
                  end;
                end;
                if not inserted then
                  insertAt(3,length(sides[3]));
              end;
            end;
          end; //autodistance<>0
        end; //haslinktoblock
      end; //link enum

      //all links have been enumerated and sorted

      if b.AutoSideDistance>=0 then
      begin
        //calculate which line comes in front of which one
        for j:=0 to 3 do
        begin
          for k:=0 to length(sides[j])-1 do
          begin
            sides[j][k].bsd.sideposition:=trunc((b.AutoSideDistance*k) - ((b.AutoSideDistance*(length(sides[j])-1)) / 2));
            sides[j][k].link.updateSide(sides[j][k].bsd);
          end;

        end;
      end;
    end;
  end;

end;

procedure TDiagram.NotifyBlockDestroy(sender: TObject);
var
  l: TDiagramLink;
  b: TDiagramBlock;
  i: integer;
begin
  if sender is TDiagramBlock then
  begin
    //delete links that pointed to this block
    b:=TDiagramBlock(sender);

    i:=0;
    if links<>nil then
    begin
      while i<links.count do
      begin
        l:=TDiagramLink(links[i]);
        if l.hasLinkToBlock(b) then
        begin
          l.free;
          links.Delete(i);
        end
        else
          inc(i);
      end;
    end;
  end;
end;

procedure TDiagram.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  b:TDiagramBlock;
  side: TDiagramBlockSide;
  l: TDiagramLink;

  bsd: TDiagramBlockSideDescriptor;

  pointindex: integer;

  startedresizing: boolean;
begin
  inherited mousedown(Button, Shift, X,Y);

  startedresizing:=false;
  for i:=blocks.count-1 downto 0 do
  begin
    b:=TDiagramBlock(blocks[i]);
    if fAllowUserToMoveBlocks and b.IsInsideHeader(x,y) then
    begin
      draggedBlock.Block:=b;
      draggedBlock.Point:=point(x-b.x,y-b.y);
      exit;
    end
    else
    if allowUserToResizeBlocks and b.IsAtBorder(x,y,side) then
    begin
      resizing.block:=b;
      resizing.side:=side;
      startedresizing:=true; //just gotta check the lines first

      if allowUserToChangeAttachPoints then
        break
      else
        exit;
    end;
  end;

  for i:=links.count-1 downto 0 do
  begin
    l:=TDiagramLink(links[i]);

    if AllowUserToMovePlotPoints then
    begin
      PointIndex:=l.getPointIndexAt(x,y);
      if pointindex<>-1 then
      begin
        draggedPoint.Link:=l;
        draggedPoint.PointIndex:=PointIndex;
        exit;
      end;
    end;

    if allowUserToChangeAttachPoints and l.isAtAttachPoint(x,y, bsd) then
    begin
      if startedresizing then resizing.block:=nil; //nope, attachpoint changing takes priority
      draggedSideAttachPoint.block:=bsd.block;
      draggedSideAttachPoint.link:=l;
      exit;
    end;

    if allowUserToCreatePlotPoints and l.isOverLine(x,y) then
    begin
      l.createPoint(point(x,y));

      pointindex:=l.getPointIndexAt(x,y);
      if pointindex<>-1 then
      begin
        draggedPoint.Link:=l;
        draggedPoint.PointIndex:=pointindex;
      end;

      repaint;

      exit;
    end;
  end;


end;

procedure TDiagram.updateAttachPointDragPosition(xpos,ypos: integer);
var b: TDiagramBlockSideDescriptor;
begin
  //get the nearest attachpoint
  b:=draggedSideAttachPoint.block.getClosestSideDescriptor(xpos,ypos);
  if b.block=nil then exit; //don't update

  draggedSideAttachPoint.link.updateSide(b);

  repaint;
end;

procedure TDiagram.updateBlockDragPosition(xpos,ypos: integer);
begin
  draggedBlock.Block.x:=xpos-draggedBlock.point.x;
  draggedBlock.Block.y:=ypos-draggedBlock.point.y;
  DoAutoSideUpdate;
  repaint;
end;

procedure TDiagram.updatePointDragPosition(xpos,ypos: integer);
begin
  if draggedPoint.link<>nil then
  begin
    draggedPoint.link.updatePointPosition(draggedPoint.pointindex, point(xpos,ypos));
    DoAutoSideUpdate;
    repaint;


  end;
end;

procedure TDiagram.updateResizePosition(xpos,ypos: integer);
begin
  if resizing.block=nil then exit;

  resizing.block.Resize(xpos,ypos,resizing.side);
  DoAutoSideUpdate;
  repaint;
end;

procedure TDiagram.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
var
  i: integer;
  pt: tpoint;
begin
  inherited WMLButtonDBLCLK(message);

  pt := GetMousePosFromMessage(Message.Pos);

  for i:=blocks.count-1 downto 0 do
  begin
    if TDiagramBlock(blocks[i]).isInside(pt.x,pt.y) then
    begin
      TDiagramBlock(blocks[i]).DblClick(pt.x,pt.y);
      exit;
    end;
  end;


end;

procedure TDiagram.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited mouseup(Button, Shift, X,Y);
  if draggedblock.block<>nil then
  begin
    updateBlockDragPosition(x,y);
    draggedBlock.block:=nil;
  end;

  if resizing.block<>nil then
  begin
    updateResizePosition(x,y);
    resizing.block:=nil;
  end;

  if draggedpoint.link<>nil then
  begin
    updatePointDragPosition(x,y);
    draggedpoint.link:=nil;
  end;

  if draggedSideAttachPoint.link<>nil then
  begin
    updateAttachPointDragPosition(x,y);
    draggedSideAttachPoint.link:=nil;
    draggedSideAttachPoint.block:=nil;
  end;


end;

procedure TDiagram.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  b: TDiagramBlock;
  borderside: TDiagramBlockSide;
  bsd: TDiagramBlockSideDescriptor;
  l: TDiagramLink;

  newcursor: TCursor;
begin
  inherited mousemove(Shift, X,Y);

  newcursor:=crdefault;

  if draggedblock.block<>nil then
    updateBlockDragPosition(x,y);

  if resizing.block<>nil then
    updateResizePosition(x,y);

  if draggedPoint.link<>nil then
    updatePointDragPosition(x,y);

  if draggedSideAttachPoint.link<>nil then
    updateAttachPointDragPosition(x,y);


  //check if the mouse is over a block
  for i:=blocks.count-1 downto 0 do
  begin
    b:=TDiagramBlock(blocks[i]);
    if AllowUserToMoveBlocks and b.IsInsideHeader(x,y) then
    begin
      newcursor:=crHandPoint;
      break;
    end
    else
    if AllowUserToResizeBlocks and b.IsAtBorder(x,y,borderside) then
    begin
      case borderside of
        dbsTopLeft:     newcursor:=crSizeNW;
        dbsTop:         newcursor:=crSizeN;
        dbsTopRight:    newcursor:=crSizeNE;
        dbsRight:       newcursor:=crSizeE;
        dbsBottomRight: newcursor:=crSizeSE;
        dbsBottom:      newcursor:=crSizeS;
        dbsBottomLeft:  newcursor:=crSizeSW;
        dbsLeft:        newcursor:=crSizeW;
      end;
      break;
    end;
  end;

  //check if over a line
  for i:=links.count-1 downto 0 do
  begin
    l:=TDiagramLink(links[i]);
    if AllowUserToMovePlotPoints and (l.getPointIndexAt(x,y)<>-1) then
    begin
      newcursor:=crSize;
      break;
    end;


    if AllowUserToCreatePlotPoints and l.isOverLine(x,y) then
    begin
      newcursor:=crCross;

      if allowUserToChangeAttachPoints and l.isAtAttachPoint(x,y,bsd) then
        newcursor:=crSize;

      break;
    end;
  end;

  if newcursor<>cursor then
    cursor:=newcursor;
end;

procedure TDiagram.paint;
var i: integer;
begin
  inherited paint;

  color:=diagramConfig.backgroundColor;

  if diagramConfig.canvas=nil then
    diagramConfig.canvas:=canvas;

  //draw the lines
  for i:=0 to links.count-1 do
    TDiagramLink(links[i]).render;


  //draw the visible blocks
  for i:=0 to blocks.count-1 do
    TDiagramBlock(blocks[i]).render;



end;

function TDiagram.createBlock: TDiagramBlock;
var b: TDiagramBlock;
begin
  b:=TDiagramBlock.create(diagramConfig);
  b.OnDestroy:=@NotifyBlockDestroy;
  blocks.Add(b);

  result:=b;
end;

function TDiagram.addConnection(originBlock, destinationBlock: TDiagramBlock): TDiagramLink;
var
  o,d: TDiagramBlockSideDescriptor;
  l: TDiagramlink;
begin
  originblock.AutoSide:=true;
  destinationblock.autoside:=true;

  o.block:=originBlock;
  o.side:=dbsTop;
  o.sideposition:=0;

  d.block:=destinationblock;
  d.side:=dbsTop;
  d.sideposition:=0;
  l:=TDiagramLink.create(diagramConfig,o, d);

  links.add(l);
  result:=l;
end;

function TDiagram.addConnection(origin, destination: TDiagramBlockSideDescriptor): TDiagramLink;
var l: TDiagramLink;
begin
  l:=TDiagramLink.create(diagramConfig,origin, destination);
  links.add(l);

  result:=l;
end;

procedure TDiagram.getConnectionsToBlock(b: TDiagramBlock; list: TList);
var
  i: integer;
begin
  for i:=0 to links.count-1 do
    if TdiagramLink(links[i]).getDestinationDescriptor.block=b then
      list.add(links[i]);
end;

procedure TDiagram.getConnectionsFromBlock(b: TDiagramBlock; list: TList);
var
  i: integer;
begin
  for i:=0 to links.count-1 do
    if TdiagramLink(links[i]).getOriginDescriptor.block=b then
      list.add(links[i]);
end;

constructor TDiagram.Create(TheOwner: TComponent);
begin
  diagramConfig:=TDiagramConfig.create(self);

  blocks:=tlist.create;
  links:=tlist.create;

  fAllowUserToCreatePlotPoints:=true;
  fAllowUserToMovePlotPoints:=true;
  fAllowUserToResizeBlocks:=true;
  fAllowUserToMoveBlocks:=true;
  fAllowUserToChangeAttachPoints:=true;

  //scrollx:=10;
  inherited create(TheOwner);
end;

destructor TDiagram.Destroy;
var i: integer;
begin
  for i:=0 to links.count-1 do
    TDiagramLink(links[i]).free;

  links.free;
  links:=nil;

  for i:=0 to blocks.count-1 do
    TDiagramBlock(blocks[i]).free;

  blocks.Free;
  blocks:=nil;

  inherited destroy;
end;

end.

