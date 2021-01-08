unit newScrollBar;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls;

type
  TNewScrollBar=class(TScrollBar)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls;


procedure TNewScrollBar.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode and (Parent<>nil) then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(handle, 'Explorer', nil);
  end;
end;

end.

