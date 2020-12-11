unit newTabControl;

//ugh

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, Forms, ComCtrls,LCLType, UxTheme, themes;

type
  TNewNoteBookStringsTabControl=class(TNoteBookStringsTabControl)
  private
    procedure DrawItem(var msg: TMessage); message WM_DRAWITEM;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

  TNewTabControlNoteBookStrings=class(TTabControlNoteBookStrings)
  protected
    function GetInternalTabControllClass: TNoteBookStringsTabControlClass; override;
  end;

  TNewTabControl=class(TTabControl)
  private
  protected
    function CreateTabNoteBookStrings: TTabControlNoteBookStrings; override;
    procedure PaintWindow(DC: HDC); override;
  public
  end;


implementation

uses Graphics, betterControls;

procedure TNewNoteBookStringsTabControl.DrawItem(var msg: TMessage);
var
  dis: PDrawItemStruct;
  c: tcanvas;
  ts:TTextStyle;
  o: TObject;

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
    c.TextRect(dis^.rcItem, 0,0,TTabSheet(o).caption,ts);

  c.free;

  msg.Result:=1;
end;

procedure TNewNoteBookStringsTabControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

  if ShouldAppsUseDarkMode() then
    params.Style:=params.style or TCS_OWNERDRAWFIXED
end;


function TNewTabControlNoteBookStrings.GetInternalTabControllClass: TNoteBookStringsTabControlClass;
begin
  Result := TNewNoteBookStringsTabControl;
end;

procedure TNewTabControl.PaintWindow(DC: HDC);
var c: tcanvas;
  r:trect;
begin
  if ShouldAppsUseDarkMode() then
  begin
    c:=tcanvas.create;
    c.handle:=dc;
    c.brush.color:=ColorSet.TextBackground;
    c.FillRect(c.ClipRect);

    //c.pen.color:=clBlue;
    c.Rectangle(rect(0,0,ClientWidth,ClientHeight));

    c.free;
  end
  else
    inherited PaintWindow(dc);
end;

function TNewTabControl.CreateTabNoteBookStrings: TTabControlNoteBookStrings;
begin
  if ShouldAppsUseDarkMode() then
    result:=TNewTabControlNoteBookStrings.create(self)
  else
    result:=inherited CreateTabNoteBookStrings;
end;

end.
