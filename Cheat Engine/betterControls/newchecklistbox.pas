unit newCheckListBox;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, CheckLst, controls, messages, lmessages,
  Win32Extra, LCLClasses,LCLProc;

type
  TNewCheckListBox=class(TCheckListbox)
  private
  protected
    procedure ChildHandlesCreated; override;

  public
  end;


implementation

uses graphics, Menus, Win32WSMenus, betterControls;

procedure TNewCheckListBox.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;



  if ShouldAppsUseDarkMode() then
  begin
    AllowDarkModeForWindow(handle,1);
    SetWindowTheme(Handle, 'explorer', nil);

    //font.color:=colorset.FontColor;
    Color:=colorset.TextBackground;
  end;
end;


end.

