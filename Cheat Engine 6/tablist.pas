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

procedure TTablist.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
var i: integer;
begin
  //find what is selected
  i:=x div fTabWidth;

  if i<fTabs.count then
    selectedTab:=i;

  inherited MouseDown(button,shift,x,y);
end;

procedure TTablist.RemoveTab(i: integer);
{
Assuming that the tabdata is already freed
}
begin
  ftabs.Delete(i);
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
var i: integer;
selectedoffset: integer;
gradientColor: Tcolor;
oldstyle: TBrushStyle;

begin
  inherited Paint;



  //create a total of 'fTabs.count' tabs
  for i:=0 to fTabs.count-1 do
  begin
    if i=fselectedTab then
    begin
      selectedoffset:=1;
      gradientColor:=color;
    end
    else
    begin
      selectedoffset:=0;
      gradientColor:=$d0d0d0;
    end;

    Canvas.Pen.Color:=$a0a0a0;
    canvas.Rectangle(i*ftabWidth,0,ftabWidth+i*ftabWidth,height);
    Canvas.GradientFill(rect(i*ftabWidth+1,1,(ftabWidth-1)+i*ftabWidth,height-1),clWhite,gradientColor, gdVertical);

    oldstyle:=canvas.Brush.Style;
    canvas.Brush.Style:=bsClear;
    Canvas.TextOut(i*ftabWidth+((ftabwidth div 2)-(canvas.TextWidth(ftabs[i]) div 2)) ,(height div 2)-(canvas.TextHeight(ftabs[i]) div 2),fTabs[i]);
    canvas.Brush.Style:=OldStyle;
  end;

  //self.Canvas.TextOut(0,0,'Fag');
  canvas.Pen.Color:=$808080;
  canvas.Line(0,height-1,width,height-1);

  canvas.Pen.Color:=color;
  Canvas.Line(fselectedTab*ftabWidth,height-1,fselectedTab*ftabwidth+ftabwidth,height-1);
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

