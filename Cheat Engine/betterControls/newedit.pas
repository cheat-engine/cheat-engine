unit newEdit;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls;

type
  TNewEdit=class(TEdit)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls;


procedure TNewEdit.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;

  if ShouldAppsUseDarkMode then
  begin
    if Parent<>nil then
    begin
      AllowDarkModeForWindow(handle, 1);
      SetWindowTheme(handle, 'CFD', nil);

      //font.color:=ColorSet.FontColor;
      color:=ColorSet.TextBackground;
    end;

  end;
end;

end.

