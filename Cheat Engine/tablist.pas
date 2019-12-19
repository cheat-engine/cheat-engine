unit tablist;

{$mode delphi}

interface

uses
  Classes, SysUtils,controls,ExtCtrls,graphics, math;

type
  TTabChangeEvent=procedure(sender: TObject; oldselection: integer) of object;

  TTablist=class;

  TControlWithArrows=class(TCustomControl)
  private
    tablist: TTablist;
  protected
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    leftArrowActive: boolean;
    rightArrowActive: boolean;
    arrowWidth: integer;
    procedure Paint; override;
  end;

  TTablist=class(TCustomControl)
  private
    fTabs: tstringlist;
    fmintabWidth: integer;
    fselectedTab: integer;
    fOnTabChange: TTabChangeEvent;

    ftabData: array of pointer;

    offset: integer; //defines how many tabs must be shifted to the left
    controlWithArrows: TControlWithArrows;

    function getTabWidth(i: integer): integer;
    function getTabXPos(i: integer): integer;

    procedure setSelectedTab(i: integer);
    function getTabData(i: integer):pointer;
    procedure setTabData(i: integer; p: pointer);
    function getCount: integer;
    function getTabText(i: integer): string;
    procedure setTabText(i: integer; s: string);
  protected
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    function AddTab(t: string):integer;
    function GetTabIndexAt(x,y: integer): integer;
    procedure RemoveTab(i: integer);
    procedure goLeft();
    procedure goRight();
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TabData[Index: Integer]: pointer read getTabData write setTabData;
  published
    property MinTabWidth: integer read fMinTabWidth write fMinTabWidth;
    property OnTabChange: TTabChangeEvent read fOnTabChange write fOnTabChange;
    property SelectedTab: integer read fSelectedTab write setSelectedTab;
    property TabText[Index: Integer]: string read getTabText write setTabText;
    property Count: integer read getCount;
end;

implementation

function TTablist.getTabText(i: integer): string;
begin
  result:=fTabs[i];
end;

procedure TTablist.setTabText(i: integer; s: string);
begin
  fTabs[i]:=s;
  invalidate;
  repaint;
end;

function TTablist.getCount: integer;
begin
  result:=fTabs.count;
end;

function TTablist.getTabData(i: integer):pointer;
begin
  result:=nil;
  if i>fTabs.count then exit;

  result:=ftabData[i];
end;

procedure TTablist.setTabData(i: integer; p: pointer);
begin
  if i>fTabs.count then exit;

  fTabData[i]:=p;
end;

procedure TTablist.setSelectedTab(i: integer);
var old: integer;
begin
  old:=fSelectedTab;
  fSelectedTab:=i;

  invalidate;
  repaint;

  if (old<>fSelectedTab) and (assigned(fOnTabChange)) then
    fOnTabChange(self,old);
end;

function TTablist.getTabXPos(i: integer): integer;
var j: integer;
begin
  result:=0;
  for j:=0 to i-1 do
    inc(result, getTabWidth(j));
end;

function TTablist.GetTabIndexAt(x,y: integer): integer;
{
Returns the index of the tab at position x,y
If no tab, return -1
}
var
  i: integer;
  startx, stopx: integer;
begin
  result:=-1;

  startx:=0;

  for i:=offset to fTabs.Count-1 do
  begin
    stopx:=startx+getTabWidth(i);

    if InRange(x, startx, stopx) then exit(i);

    startx:=stopx;
  end;
end;

procedure TTablist.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var i: integer;
begin
  i:=GetTabIndexAt(x,y);
  if i<>-1 then
    selectedTab:=i;

  inherited MouseDown(button,shift,x,y);
end;

procedure TTablist.RemoveTab(i: integer);
{
Assuming that the tabdata is already freed
}
var j: integer;
begin
  ftabs.Delete(i);
  for j:=i to length(ftabdata)-2 do
    ftabdata[j]:=ftabdata[j+1];

  //do a tabswitch without calling the onchange
  if fselectedTab=i then //if for some reason the current tab was deleted
    fselectedtab:=-1
  else
  begin
    if fselectedtab>i then
      fselectedtab:=fselectedtab-1;
  end;

  invalidate;
  repaint;
end;

function TTablist.AddTab(t: string): integer;
begin
  fTabs.Add(t);
  setlength(ftabData,fTabs.count);
  result:=ftabs.count-1;
  invalidate;
  repaint;
end;

function TTabList.GetTabWidth(i: integer): integer;
begin
  result:=max(fmintabWidth, canvas.GetTextWidth(fTabs[i])+8);
end;

procedure TTablist.Paint;
var
  i,j: integer;
  gradientColor: Tcolor;
  oldstyle: TBrushStyle;

  tabwidth: integer;

  lastx: integer;

  selectedx: integer;


begin
  inherited Paint;

  selectedx:=0;
  lastx:=0;

  //create a total of 'fTabs.count' tabs
  for j:=offset to fTabs.count-1 do
  begin
    i:=j-offset;
    tabwidth:=GetTabWidth(j);

    if j=fselectedTab then
    begin
      selectedx:=lastx;
      gradientColor:=color;
    end
    else
    begin
      gradientColor:=$d0d0d0;
    end;

    Canvas.Pen.Color:=$a0a0a0;
    canvas.Rectangle(lastx,0,lastx+tabWidth,height);
    Canvas.GradientFill(rect(lastx+1,1,lastx+tabwidth-1,height-1),clWhite,gradientColor, gdVertical);

    oldstyle:=canvas.Brush.Style;

    canvas.Brush.Style:=bsClear;
    Canvas.TextRect(rect(lastx, 0, lastx+tabWidth,height),lastx+((tabwidth div 2) - (canvas.TextWidth(ftabs[j]) div 2)),(height div 2)-(canvas.TextHeight(ftabs[j]) div 2),fTabs[j]);
    canvas.Brush.Style:=OldStyle;

    inc(lastx, tabwidth);
  end;

  canvas.Pen.Color:=$808080;
  canvas.Line(0,height-1,width,height-1);

  canvas.Pen.Color:=color;
  if fselectedTab>=offset then Canvas.Line(selectedx,height-1,selectedx+getTabWidth(fselectedTab),height-1);


  if (offset>0) or (lastx>width) then //if there are more tabs than visible
  begin

    if controlWithArrows.Parent<>self.Parent then // ensure parent is the same
    begin                                         // (in case user decides to move tablist control)
      controlWithArrows.Parent:=self.Parent;
      controlWithArrows.arrowWidth:=(height div 6) * 5;
      controlWithArrows.Width:=controlWithArrows.arrowWidth*2+2;
      controlWithArrows.Height:=height;

      if self.Top<height then // check if there is room for it
      begin
        controlWithArrows.AnchorSideBottom.Side:=asrBottom;
        controlWithArrows.BorderSpacing.Right:=0;
      end;

      controlWithArrows.Invalidate;
    end;

    controlWithArrows.Visible:=true;

    if lastx>width then
      controlWithArrows.rightArrowActive:=true
    else
      controlWithArrows.rightArrowActive:=false;

    if (offset>0) then //can you scroll to the left
      controlWithArrows.leftArrowActive:=true
    else
      controlWithArrows.leftArrowActive:=false;

    controlWithArrows.Repaint

  end
  else
    controlWithArrows.Visible:=false;
end;

procedure TTablist.goLeft();
var i: integer;
begin
  //check if you can go left
  i:=ftabs.count-1;

  if getTabXPos(i-offset)+getTabWidth(i)>width then
    inc(offset);

  Repaint;
end;

procedure TTablist.goRight();
begin
  if offset>0 then
    dec(offset);

  Repaint;
end;

constructor TTablist.Create(AOwner: TComponent);
begin
  Inherited create(AOwner);
  fselectedTab:=0;
  fTabs:=TStringlist.create;
  fMinTabWidth:=80;

  controlWithArrows:=TControlWithArrows.Create(self);
  controlWithArrows.Visible:=false;
  controlWithArrows.tablist:=self;
  controlWithArrows.Anchors:=[akBottom,akRight];
  controlWithArrows.AnchorSideBottom.Control:=Self;
  controlWithArrows.AnchorSideBottom.Side:=asrTop;
  controlWithArrows.AnchorSideRight.Control:=Self;
  controlWithArrows.AnchorSideRight.Side:=asrRight;
  controlWithArrows.BorderSpacing.Right:=50;
end;

destructor TTablist.Destroy;
begin
  if fTabs<>nil then
    ftabs.free;

  inherited Destroy;
end;

procedure TControlWithArrows.Paint;
begin
  inherited Paint;

  if rightArrowActive then
  begin
    canvas.Pen.Color:=clred;
    canvas.Brush.color:=clblue;
  end
  else
  begin
    Canvas.pen.color:=clInactiveBorder;
    Canvas.brush.color:=clInactiveCaption;
  end;
  canvas.Polygon([point(Width-arrowWidth, 2), point(Width-arrowWidth, Height-2), point(Width-1, (Height div 2))]);

  if leftArrowActive then
  begin
    canvas.Pen.Color:=clred;
    canvas.Brush.color:=clblue;
  end
  else
  begin
    Canvas.pen.color:=clInactiveBorder;
    Canvas.brush.color:=clInactiveCaption;
  end;
  canvas.Polygon([point(Width-(arrowWidth+2), 2), point(Width-(arrowWidth+2), Height-2), point(Width-(arrowWidth*2+2), (Height div 2))]);

  canvas.Pen.Color:=$808080;
  canvas.Line(0,height-1,0,1);
  canvas.LineTo(width-1,1);
  canvas.LineTo(width-1,height-1);
end;

procedure TControlWithArrows.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  //clicked on an arrow
  if x>Width-arrowWidth-1 then
    tablist.goLeft()
  else
    tablist.goRight();
end;

end.

