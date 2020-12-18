unit newGroupBox; //for some reason parentfont does not work

{$mode objfpc}{$H+}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, StdCtrls, LCLType,
  lmessages,graphics, ExtCtrls;

type
  TNewGroupBox=class(TGroupBox)
  private
  protected

    procedure CreateParams(var Params: TCreateParams); override;

  // procedure CreateBrush; override;

    procedure ChildHandlesCreated; override;
    procedure PaintControls(DC: HDC; First: TControl);

  public

  end;


implementation

uses betterControls, WSLCLClasses, WSStdCtrls, Win32Proc, Win32Int;

var
  OriginalGroupBoxHandler: ptruint;

function GroupBoxSubClass(wnd:HWND; msg:UINT; _wparam:WPARAM; _lparam:LPARAM):LRESULT; stdcall;
var
  Info: PWin32WindowInfo;
  ps: windows.TPaintStruct;
  dc: hdc;

  c: TCanvas;

  gb: TNewGroupBox;
  r:trect;
  h: LPNMHDR;
  i: integer;
  p: tpoint;
begin

  if msg=WM_PAINT then
  begin
    Info := GetWin32WindowInfo(wnd);
    if (info<>nil) and (info^.WinControl is TNewGroupBox) then
    begin
      gb:=TNewGroupBox(info^.WinControl);

      dc:=BeginPaint(wnd, ps);
      if dc<>0 then
      begin

        c:=tcanvas.create;
        c.handle:=dc;
        c.brush.style:=bsSolid;

        if (gb.color=clDefault) and gb.ParentColor then
          c.brush.color:=gb.GetRGBColorResolvingParent
        else
          c.brush.color:=gb.color;

        c.Pen.color:=clWindowFrame;
        c.FillRect(ps.rcPaint);


        r:=rect(0,c.TextHeight('Q') div 2,gb.width,gb.height);
        c.brush.Style:=bsClear;
        c.Rectangle(r);

        c.font.color:=gb.font.color;
        c.brush.Style:=bsSolid;

        c.TextOut(10,0,gb.Caption);
        c.free;


        p:=gb.ScreenToControl(gb.ClientToScreen(point(0,0)));
        MoveWindowOrg(dc,p.x, p.y);

        gb.paintcontrols(dc,gb.controls[0]);

        EndPaint(wnd,ps);
        exit(0);
      end;


    end;

  end;

  result:=CallWindowProc(WNDPROC(OriginalGroupBoxHandler), wnd, msg, _wparam, _lparam);
end;

procedure TNewGroupBox.PaintControls(DC: HDC; First: TControl);
begin
  inherited PaintControls(DC, First);
end;

procedure TNewGroupBox.ChildHandlesCreated;
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

procedure TNewGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

end;

end.

