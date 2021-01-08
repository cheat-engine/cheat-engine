unit newDirectoryEdit;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls,EditBtn;

type
  TNewDirectoryEdit=class(TDirectoryEdit)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls;


procedure TNewDirectoryEdit.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode and (Parent<>nil) then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(handle, 'CFD', nil);

    //font.color:=ColorSet.FontColor;
    color:=ColorSet.TextBackground;
  end;
end;

end.

