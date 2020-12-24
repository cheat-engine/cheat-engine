unit newColorBox;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, stdctrls, controls, messages, lmessages,
  Win32Extra, LCLClasses,LCLProc, colorbox;

type
  TNewColorBox=class(TColorBox)
  private
    creatingBrush: boolean;
  protected
    procedure CreateBrush; override;
  public
  end;


implementation

uses graphics, Menus, Win32WSMenus, betterControls, newComboBox;




procedure TNewColorBox.CreateBrush;
var
  cbi: TComboBoxINFO;
begin
  if ShouldAppsUseDarkMode() then
  begin
    if creatingBrush then
    begin
      inherited createbrush;
      exit;
    end;

    creatingbrush:=true;
    cbi.cbSize:=sizeof(cbi);
    if GetComboBoxInfo(handle, @cbi) then
    begin

      AllowDarkModeForWindow(cbi.hwndCombo,1);
      AllowDarkModeForWindow(cbi.hwndItem,1);
      AllowDarkModeForWindow(cbi.hwndList,1);

      SetWindowTheme(cbi.hwndCombo, 'cfd', nil);
      SetWindowTheme(cbi.hwndItem, 'cfd', nil);
      SetWindowTheme(cbi.hwndList, 'explorer', nil);

      if OriginalComboboxListHandler=0 then
        OriginalComboboxListHandler:=SetWindowLongPtr(cbi.hwndCombo, GWLP_WNDPROC, UINT_PTR(@ComboboxListSubClass))
      else
        SetWindowLongPtr(cbi.hwndCombo, GWLP_WNDPROC, UINT_PTR(@ComboboxListSubClass));

    end;

    inherited CreateBrush;


    brush.color:=ColorSet.EditBackground;
    font.color:=colorset.FontColor;

    if comboboxdefaultBrush=nil then
    begin
      comboboxdefaultBrush:=TBrush.Create;
      comboboxdefaultBrush.Assign(brush);
    end;

    creatingbrush:=false;
  end
  else
    inherited createbrush;
end;



end.

