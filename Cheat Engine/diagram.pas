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

    graphConfig: TDiagramConfig;
    procedure NotifyBlockDestroy(sender: TObject);
    procedure updateBlockDragPosition(xpos,ypos: integer);
    procedure updateResizePosition(xpos,ypos: integer);
    procedure updatePointDragPosition(xpos,ypos: integer);
    procedure updateAttachPointDragPosition(xpos,ypos: integer);
    function getLineThickness: integer;
    procedure setLineThickness(t: integer);
    function getDefaultLineColor: TColor;
    procedure setDefaultLineColor(c: tcolor);
    function getDrawPlotPoints: boolean;
    procedure setDrawPlotPoints(state: boolean);
    function getDefaultPlotPointColor: TColor;
    procedure setDefaultPlotPointColor(c: tcolor);

    function getArrowStyles: TArrowStyles;
    procedure setArrowStyles(s: TArrowStyles);

    function getBlockBackground: TColor;
    procedure setBlockBackground(c: TColor);
  protected
    procedure paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    //procedure DblClick; override;
  public
    function createBlock: TDiagramBlock;
    function addConnection(origin, destination: TDiagramBlockSideDescriptor): TDiagramLink;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineThickness: integer read getLineThickness write setLineThickness;
    property DefaultLineColor: Tcolor read getDefaultLineColor write setDefaultLineColor;
    property DrawPlotPoints: boolean read getDrawPlotPoints write setDrawPlotPoints;
    property DefaultPlotPointColor: TColor read getDefaultPlotPointColor write setDefaultPlotPointColor;
    property AllowUserToCreatePlotPoints: boolean read fAllowUserToCreatePlotPoints write fAllowUserToCreatePlotPoints;
    property AllowUserToMovePlotPoints: boolean read fAllowUserToMovePlotPoints write fAllowUserToMovePlotPoints;
    property AllowUserToResizeBlocks: boolean read fAllowUserToResizeBlocks write fAllowUserToResizeBlocks;
    property AllowUserToMoveBlocks: boolean read fAllowUserToMoveBlocks write fAllowUserToMoveBlocks;
    property allowUserToChangeAttachPoints: boolean read fallowUserToChangeAttachPoints write fallowUserToChangeAttachPoints;

    property ArrowStyle: TArrowStyles read getArrowStyles write setArrowStyles;
    property BlockBackground: TColor read getBlockBackground write setBlockBackground;
  end;

implementation

function TDiagram.getDefaultPlotPointColor: TColor;
begin
  result:=graphconfig.PlotPointColor;
end;

procedure TDiagram.setDefaultPlotPointColor(c: tcolor);
begin
  graphconfig.PlotPointColor:=c;
  if parent<>nil then
    repaint;
end;

function TDiagram.getArrowStyles: TArrowStyles;
begin
  result:=graphconfig.ArrowStyles;
end;

procedure TDiagram.setArrowStyles(s: TArrowStyles);
begin
  graphconfig.ArrowStyles:=s;
  if parent<>nil then
    repaint;
end;

function TDiagram.getDrawPlotPoints: boolean;
begin
  result:=graphconfig.drawPlotPoints;
end;

procedure TDiagram.setDrawPlotPoints(state: boolean);
begin
  graphconfig.drawPlotPoints:=state;
  if parent<>nil then
    repaint;
end;

function TDiagram.getDefaultLineColor: tcolor;
begin
  result:=graphconfig.LineColor;
end;

procedure TDiagram.setDefaultLineColor(c: tcolor);
begin
  graphconfig.linecolor:=c;
  if parent<>nil then
    repaint;
end;

function TDiagram.getLineThickness: integer;
begin
  result:=graphconfig.LineThickness;
end;

procedure TDiagram.setLineThickness(t: integer);
begin
  graphconfig.LineThickness:=t;
  if parent<>nil then
    repaint;
end;

function TDiagram.getBlockBackground: TColor;
begin
  result:=graphconfig.BlockBackground;
end;

procedure TDiagram.setBlockBackground(c: TColor);
begin
  graphconfig.BlockBackground:=c;
  if parent<>nil then
    repaint;
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
  repaint;
end;

procedure TDiagram.updatePointDragPosition(xpos,ypos: integer);
begin
  if draggedPoint.link<>nil then
  begin
    draggedPoint.link.updatePointPosition(draggedPoint.pointindex, point(xpos,ypos));
    repaint;
  end;
end;

procedure TDiagram.updateResizePosition(xpos,ypos: integer);
var dx,dy: integer;
begin
  if resizing.block=nil then exit;

  resizing.block.Resize(xpos,ypos,resizing.side);

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

  if graphconfig.canvas=nil then
    graphconfig.canvas:=canvas;

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
  b:=TDiagramBlock.create(graphconfig);
  b.OnDestroy:=@NotifyBlockDestroy;
  blocks.Add(b);

  result:=b;
end;

function TDiagram.addConnection(origin, destination: TDiagramBlockSideDescriptor): TDiagramLink;
var l: TDiagramLink;
begin
  l:=TDiagramLink.create(graphconfig,origin, destination);
  links.add(l);

  result:=l;
end;

constructor TDiagram.Create(TheOwner: TComponent);
begin
  graphConfig:=TDiagramConfig.create(canvas);

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
begin
  blocks.Free;
end;

end.

