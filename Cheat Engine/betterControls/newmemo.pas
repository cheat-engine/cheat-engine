unit newMemo;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, stdctrls, controls, messages, lmessages,
  Win32Extra, LCLClasses,LCLProc;

type
  TNewMemo=class(TMemo)
  private
  protected
    procedure ChildHandlesCreated; override;

  public
  end;


implementation

uses graphics, Menus, Win32WSMenus, betterControls;

procedure TNewMemo.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;

  if ShouldAppsUseDarkMode and (parent<>nil) then
  begin
    AllowDarkModeForWindow(handle,1);
    SetWindowTheme(Handle, 'explorer', nil);

    if ShouldAppsUseDarkMode() then
    begin
     // font.color:=colorset.FontColor;
      Color:=colorset.TextBackground;
    end;

  end;
end;


end.

