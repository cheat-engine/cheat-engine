unit newListView;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, ComCtrls, Controls, messages, lmessages, graphics,
  CommCtrl;

type
  TNewListView=class(ComCtrls.TListView)
  private
    fDefaultBackgroundColor: COLORREF;
    fDefaultTextColor: COLORREF;
    procedure setViewStyle(style: TViewStyle);
    function getViewStyle: TViewStyle;
    procedure pp(var msg: TMessage); message WM_NOTIFY;
  protected
    procedure ChildHandlesCreated; override;
  public
  published
    property ViewStyle: TViewStyle read getViewStyle write setViewStyle;
  end;


implementation

uses betterControls;


procedure TNewListView.setViewStyle(style: TViewStyle);
var h: THandle;
begin
  inherited ViewStyle:=style;

  if style=vsReport then
  begin
    h:=ListView_GetHeader(handle);
    if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    begin
      AllowDarkModeForWindow(h, 1);
      SetWindowTheme(h, 'ItemsView',nil);
    end;
  end;
end;

function TNewListView.getViewStyle: TViewStyle;
begin
  result:=inherited ViewStyle;
end;

procedure TNewListView.ChildHandlesCreated;
var theme: THandle;
begin
  inherited ChildHandlesCreated;
  if parent<>nil then
  begin
    AllowDarkModeForWindow(handle, 1);
    SetWindowTheme(handle, 'Explorer', nil);

    theme:=OpenThemeData(0,'ItemsView');  //yeah....why make it obvious if you can make it obscure right ?  (This is a microsoft thing, not because i'm an asshole )

    if theme<>0 then
    begin
      GetThemeColor(theme, 0,0,TMT_TEXTCOLOR,fDefaultTextColor);
      GetThemeColor(theme, 0,0,TMT_FILLCOLOR,fDefaultBackgroundColor);
      CloseThemeData(theme);
    end
    else
      fDefaultTextColor:=font.color;

    Font.color:=fDefaultTextColor;
    Color:=fDefaultBackgroundColor;

  end;
end;

procedure TNewListView.pp(var msg: TMessage);
var
  p1: LPNMHDR;
  p2: LPNMCUSTOMDRAW;
begin
  p1:=LPNMHDR(msg.lparam);
  if p1^.code=UINT(NM_CUSTOMDRAW) then
  begin
    p2:=LPNMCUSTOMDRAW(msg.lParam);


    case p2^.dwDrawStage of
      CDDS_PREPAINT: msg.Result:=CDRF_NOTIFYITEMDRAW;
      CDDS_ITEMPREPAINT:
      begin
        SetTextColor(p2^.hdc, fDefaultTextColor);
        msg.result:=CDRF_DODEFAULT;
      end;
    end;
  end;
end;


end.

