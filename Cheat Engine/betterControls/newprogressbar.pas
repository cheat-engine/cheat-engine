unit newProgressBar;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, ComCtrls, CommCtrl, controls, messages, lmessages,
  Win32Extra, LCLClasses,LCLProc;

type
  TNewProgressBar=class(TProgressBar)
  private
  protected
    procedure ChildHandlesCreated; override;

  public
  end;


implementation

uses graphics, Menus, Win32WSMenus, betterControls;

procedure TNewProgressBar.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;

  AllowDarkModeForWindow(handle,1);

  if ShouldAppsUseDarkMode() then
  begin
    SetWindowTheme(Handle,'','');
    SendMessage(Handle, PBM_SETBARCOLOR, 0,$383838);
    SendMessage(Handle, PBM_SETBKCOLOR, 0, $999999);
  end;
end;


end.

