unit newStatusBar;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls,ComCtrls, LCLType, lmessages;

type
  TNewStatusPanel=class(TStatusPanel)
  public
    constructor Create(ACollection: TCollection); override;
  end;

  TNewStatusBar=class(TStatusBar)
  private
  protected
   procedure CreateHandle; override;
   function GetPanelClass: TStatusPanelClass; override;
   procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); override;
  public
   constructor Create(TheOwner: TComponent); override;


  end;


implementation

uses betterControls, Graphics;

constructor TNewStatusPanel.Create(ACollection: TCollection);
begin
  inherited create(ACollection);

  style:=psOwnerDraw;
end;

function TNewStatusBar.GetPanelClass: TStatusPanelClass;
begin
  if ShouldAppsUseDarkMode() then
    result:=TNewStatusPanel
  else
    result:=TStatusPanel;
end;

procedure TNewStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
var ts: TTextStyle;
begin
  if ShouldAppsUseDarkMode then
  begin
    if Assigned(OnDrawPanel) then
      OnDrawPanel(self, panel, rect)
    else
    begin
      ts.Alignment:=taLeftJustify;
      ts.Layout:=tlCenter;
      canvas.font.color:=font.color;
      canvas.brush.style:=bsClear;

      canvas.fillrect(rect);

      canvas.TextRect(rect,rect.left,0,panel.Text,ts);
    end;

  end
  else
    inherited DrawPanel(panel, rect);
end;

procedure TNewStatusBar.CreateHandle;
var r: ptruint;
begin
  inherited CreateHandle;
  if ShouldAppsUseDarkMode() then
    SetWindowTheme(Handle, '', '');
end;

constructor TNewStatusBar.Create(TheOwner: TComponent);
begin
  inherited create(theowner);
  if ShouldAppsUseDarkMode() then
    SizeGrip:=false;
end;

end.

