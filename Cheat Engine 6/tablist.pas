unit tablist;

{$mode delphi}

interface

uses
  Classes, SysUtils,controls,ExtCtrls,graphics;

type
  TTabChangeEvent=procedure(sender: TObject; oldselection: integer) of object;

  TTablist=class(TCustomControl)
  private
    fTabs: tstringlist;
    ftabWidth: integer;
    fselectedTab: integer;
    fOnTabChange: TTabChangeEvent;

    ftabData: array of pointer;

    offset: integer; //defines how many tabs must be shifted to the left

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
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TabWidth: integer read fTabWidth write fTabWidth;
    property OnTabChange: TTabChangeEvent read fOnTabChange write fOnTabChange;
    property SelectedTab: integer read fSelectedTab write setSelectedTab;
    property TabData[Index: Integer]: pointer read getTabData write setTabData;
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

function TTablist.GetTabIndexAt(x,y: integer): integer;
{
Returns the index of the tab at position x,y
If no tab, return -1
}
var i: integer;
begin
  result:=-1;

  //find what is selected
  i:=offset+(x div fTabWidth);

  if i<fTabs.count then
    result:=i;
end;

procedure TTablist.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var i: integer;
begin
  if (fTabs.count*fTabWidth>width) and (x>width-30) then
  begin
    //clicked on an arrow
    if x>width-15 then
    begin
      //click right
      //check if you can go left

      if (fTabs.count-offset)*fTabWidth>width then //if you can still scroll to the right then increase offset
        inc(offset)
    end
    else
    begin
      //click left
      if offset>0 then
        dec(offset);
    end;

    Repaint;

  end
  else
  begin
    i:=GetTabIndexAt(x,y);
    if i<>-1 then
      selectedTab:=i;
  end;

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

procedure TTablist.Paint;
var i,j: integer;
gradientColor: Tcolor;
oldstyle: TBrushStyle;

begin
  inherited Paint;



  //create a total of 'fTabs.count' tabs
  for j:=offset to fTabs.count-1 do
  begin
    i:=j-offset;

    if j=fselectedTab then
    begin
      gradientColor:=color;
    end
    else
    begin
      gradientColor:=$d0d0d0;
    end;

    Canvas.Pen.Color:=$a0a0a0;
    canvas.Rectangle(i*ftabWidth,0,ftabWidth+i*ftabWidth,height);
    Canvas.GradientFill(rect(i*ftabWidth+1,1,(ftabWidth-1)+i*ftabWidth,height-1),clWhite,gradientColor, gdVertical);

    oldstyle:=canvas.Brush.Style;
    canvas.Brush.Style:=bsClear;

    Canvas.TextRect(rect(i*ftabWidth,0,ftabWidth+i*ftabWidth,height),i*ftabWidth+((ftabwidth div 2)-(canvas.TextWidth(ftabs[j]) div 2)) ,(height div 2)-(canvas.TextHeight(ftabs[j]) div 2),fTabs[j]);
    canvas.Brush.Style:=OldStyle;
  end;

  //self.Canvas.TextOut(0,0,'Fag');
  canvas.Pen.Color:=$808080;
  canvas.Line(0,height-1,width,height-1);

  canvas.Pen.Color:=color;
  Canvas.Line((fselectedTab-offset)*ftabWidth,height-1,(fselectedTab-offset)*ftabwidth+ftabwidth,height-1);

  if fTabs.count*fTabWidth>width then //if there are more tabs than visible
  begin
    if (fTabs.count-offset)*fTabWidth>width then
    begin
      canvas.Pen.Color:=clred;
      canvas.Brush.color:=clblue;
    end
    else
    begin
      Canvas.pen.color:=clInactiveBorder;
      Canvas.brush.color:=clInactiveCaption;
    end;
    canvas.Polygon([point(width-14, 2), point(width-14, height-2), point(width-1, (height div 2))]);

    if (offset>0) then //can you scroll to the left
    begin
      canvas.Pen.Color:=clred;
      canvas.Brush.color:=clblue;
    end
    else
    begin
      Canvas.pen.color:=clInactiveBorder;
      Canvas.brush.color:=clInactiveCaption;
    end;
    canvas.Polygon([point(width-16, 2), point(width-16, height-2), point(width-30, (height div 2))]);
  end;
end;

constructor TTablist.Create(AOwner: TComponent);
begin
  Inherited create(AOwner);
  fselectedTab:=0;
  fTabs:=TStringlist.create;
  tabWidth:=80;
end;

destructor TTablist.Destroy;
begin
  if fTabs<>nil then
    ftabs.free;

  inherited Destroy;
end;

end.

