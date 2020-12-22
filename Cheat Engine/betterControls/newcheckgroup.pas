unit newCheckGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TNewCheckGroup=class(TCheckGroup)
  private
  protected
    procedure ChildHandlesCreated; override;
  public
  end;


implementation

uses windows, betterControls, newGroupBox;

 
procedure TNewCheckGroup.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if ShouldAppsUseDarkMode() then
  begin

    if OriginalGroupBoxHandler=0 then
      OriginalGroupBoxHandler:=SetWindowLongPtr(handle, GWLP_WNDPROC, UINT_PTR(@GroupBoxSubClass))
    else
      SetWindowLongPtr(handle, GWLP_WNDPROC, UINT_PTR(@GroupBoxSubClass));
  end;

end;


end.

