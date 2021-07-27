unit newMainMenu;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, Menus, Graphics;

type
  TNewMenuItem=class(TMenuItem)
  private
    fCustomFontColor: TColor;
  protected
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState): Boolean; override;
    procedure setCustomFontColor(newcolor: TColor); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property FontColor: TColor read fCustomFontColor write setCustomFontColor;
  end;


  TNewMainMenu=class(TMainMenu)
  private
    procedure firstshow(sender: TObject);
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
  end;


implementation

uses betterControls, forms, LCLType;

procedure TNewMainMenu.firstshow(sender: TObject);
var m: Hmenu;
  mi: windows.MENUINFO;
  c: tcanvas;
  mia: windows.LPCMENUINFO;


 // mbi: ENUBARINFO ;

  b: TBrush;
begin
  if ShouldAppsUseDarkMode then
  begin
    m:=GetMenu(TCustomForm(sender).handle);
   // m:=handle;


    mi.cbSize:=sizeof(mi);
    mi.fMask := MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;

    b:=TBrush.Create;
    b.color:=$2b2b2b;


    b.Style:=bsSolid;

    mi.hbrBack:=b.handle; //GetSysColorBrush(DKGRAY_BRUSH); //b.Handle;
    mia:=@mi;
    if windows.SetMenuInfo(m,mia) then
    begin
      AllowDarkModeForWindow(m,1);

      SetWindowTheme(m,'',nil);
    end;

  end;
end;

procedure TNewMainMenu.SetParentComponent(Value: TComponent);
begin
  inherited SetParentComponent(value);


  if ShouldAppsUseDarkMode and (value is tcustomform) then
    tcustomform(value).AddHandlerFirstShow(@firstshow);
end;

function TNewMenuItem.DoDrawItem(ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState): Boolean;
var oldc: tcolor;

  bmp: Tbitmap;
  ts: TTextStyle;

  i: integer;
  lastvisible: integer;
begin
  result:=inherited DoDrawItem(ACanvas, ARect, AState);

  if ShouldAppsUseDarkMode() and (result=false) then
  begin
    result:=Parent.Menu is TMainMenu;
    oldc:=acanvas.Brush.color;

    if result then
    begin
      acanvas.Brush.color:=$313131;

      lastvisible:=-1;
      for i:=parent.count-1 downto 0 do
        if parent[i].Visible then
        begin
          lastvisible:=i;
          break;
        end;

      if MenuIndex=lastvisible then //name='MenuItem3' then
      begin
        if owner is TCustomForm then
          ARect.Width:=tcustomform(owner).width
        else
        begin
          ARect.Width:=acanvas.width-arect.Left;
        end;
      end;
      acanvas.FillRect(arect);


      if fCustomFontColor=clDefault then
        acanvas.font.color:=clWhite
      else
        acanvas.font.color:=fCustomFontColor;

      ts:=acanvas.TextStyle;
      ts.ShowPrefix:=true;
      acanvas.Brush.Style:=bsSolid;
      acanvas.TextRect(arect,arect.left,arect.top,caption, ts);
      acanvas.Brush.color:=oldc;
    end;

    if (not result) and (caption='-') then
    begin
      acanvas.Brush.color:=$2b2b2b;
      acanvas.FillRect(arect);
      ts:=acanvas.TextStyle;
      ts.ShowPrefix:=true;
      acanvas.Brush.Style:=bsSolid;
      acanvas.pen.color:=clGray;
      acanvas.pen.Width:=1;
      acanvas.Line(arect.left+acanvas.TextWidth(' '), arect.CenterPoint.Y,arect.right-acanvas.TextWidth(' '), arect.CenterPoint.Y);
      acanvas.Brush.color:=oldc;
      result:=true;
    end;

  end;
end;

procedure TNewMenuItem.setCustomFontColor(newcolor: TColor);
begin
  fCustomFontColor:=newcolor;
  enabled:=not enabled; //repaints it
  enabled:=not enabled;
end;

constructor TNewMenuItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCustomFontColor:=clDefault;
end;

end.

