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



  if ShouldAppsUseDarkMode() then
  begin
    AllowDarkModeForWindow(handle,1);
    SetWindowTheme(Handle,'','');
    SendMessage(Handle, PBM_SETBARCOLOR, 0,$383838);
    SendMessage(Handle, PBM_SETBKCOLOR, 0, $999999);
  end;
end;


end.

