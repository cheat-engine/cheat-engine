unit newToggleBox;

{$mode objfpc}{$H+}

interface

uses
  windows, UxTheme,Classes, SysUtils, StdCtrls;

type
  TNewToggleBox=class(StdCtrls.TToggleBox)
  protected
    procedure ChildHandlesCreated; override;
  end;

implementation

uses betterControls;

procedure TNewToggleBox.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode and (Parent<>nil) then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(handle, 'EXPLORER', nil);
  end;
end;


end.

