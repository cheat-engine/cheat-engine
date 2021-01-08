unit newSynEdit;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, synedit;

type
  TNewSynEdit=class(TSynEdit)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses betterControls;


procedure TNewSynEdit.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode and (Parent<>nil) then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(handle, 'Explorer', nil);
  end;
end;

end.

