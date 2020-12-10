unit newGroupBox; //for some reason parentfont does not work

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls;

type
  TNewGroupBox=class(TGroupBox)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls;


procedure TNewGroupBox.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if Parent<>nil then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(Handle, '', '');

    font.color:=ColorSet.FontColor;
    color:=ColorSet.TextBackground;
  end;
end;

end.

