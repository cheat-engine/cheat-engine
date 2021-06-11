unit newTreeView; //for some reason parentfont does not work

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls,  ComCtrls, CommCtrl;

type
  TNewTreeView=class(TTreeView)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls, Graphics;

procedure TNewTreeView.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;

  if ShouldAppsUseDarkMode and (parent<>nil) then
  begin
    ToolTips:=false;
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(Handle, 'explorer', nil);

    font.color:=ColorSet.FontColor;
    color:=ColorSet.TextBackground;

    TreeLineColor:=colorset.ButtonBorderColor;

    options:=options-[tvoThemedDraw];

  end;
end;


end.

