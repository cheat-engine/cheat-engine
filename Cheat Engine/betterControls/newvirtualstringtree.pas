unit newvirtualstringtree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, graphics, laz.VirtualTrees,JwaUxTheme;

type
  TNewLazVirtualStringTree=class(TLazVirtualStringTree)
  private
    procedure ohdqe(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure oahd(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
  protected
    procedure ChildHandlesCreated; override;
  end;

implementation

uses betterControls;

procedure TNewLazVirtualStringTree.ohdqe(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements:=Elements+[hpeBackground];
end;

procedure TNewLazVirtualStringTree.oahd(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if hpeBackground in elements then
  begin
    //drawing background
    if PaintInfo.Column=nil then
    begin
      paintinfo.TargetCanvas.brush.color:=inccolor(colorset.TextBackground,8);
      paintinfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
    end
    else
    begin
      paintinfo.TargetCanvas.pen.color:=colorset.ButtonBorderColor;
      paintinfo.TargetCanvas.brush.color:=inccolor(colorset.TextBackground,8);
      paintinfo.TargetCanvas.Rectangle(PaintInfo.PaintRectangle);
    end;
  end;
end;

procedure TNewLazVirtualStringTree.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;

  if ShouldAppsUseDarkMode and (parent<>nil) then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(Handle, 'explorer', nil);

    color:=colorset.TextBackground;
    font.color:=ColorSet.FontColor;

    Header.Background:=colorset.TextBackground;
    Header.Font.color:=ColorSet.FontColor;
    header.style:=hsPlates;

    header.Options:=header.Options+[hoOwnerDraw];
    OnHeaderDrawQueryElements:=@ohdqe;
    OnAdvancedHeaderDraw:=@oahd;

    colors.UnfocusedSelectionColor:=inccolor(colors.FocusedSelectionColor,8);
    colors.UnfocusedSelectionBorderColor:=inccolor(colors.FocusedSelectionBorderColor,8);
  end;
end;

end.

