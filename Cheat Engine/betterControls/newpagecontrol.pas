unit newPageControl;

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, Forms, ComCtrls,LCLType;

type
  TNewPageControl=class(TPageControl)
  private
    procedure DrawItem(var msg: TMessage); message WM_DRAWITEM;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;


implementation

uses Graphics, betterControls;

procedure TNewPageControl.DrawItem(var msg: TMessage);
var
  dis: PDrawItemStruct;
  c: tcanvas;
  ts:TTextStyle;
  o: TObject;

begin
  if ShouldAppsUseDarkMode then
  begin
    dis:=PDrawItemStruct(msg.lParam);

    o:=TObject(dis^.itemData);


    c:=tcanvas.CREATE;
    c.handle:=dis^._hDC;
    c.Brush.color:=color;
    c.FillRect(dis^.rcItem);

    c.Font.color:=font.color; //clWindowText;
    ts:=c.TextStyle;
    ts.Alignment:=taCenter;
    ts.Layout:=tlCenter;

    if o is TTabSheet then
      c.TextRect(dis^.rcItem, 0,0,ttabsheet(o).Caption,ts);

    c.free;

  end;
end;

procedure TNewPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

  if ShouldAppsUseDarkMode then
    params.Style:=params.style or TCS_OWNERDRAWFIXED;
end;

end.
