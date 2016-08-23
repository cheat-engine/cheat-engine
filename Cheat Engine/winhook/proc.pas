unit proc;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils;

function wp (hWND:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;stdcall;

implementation

uses com;

function wp (hWND:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;stdcall;
var
  r: QWORD;
  PrevWndFunc: WNDPROC absolute r;
begin
  r:=CEConnection.DoCommand('return CEWindowProcEvent_Internal('+IntToStr(hWnd)+','+IntToStr(Msg)+','+IntToStr(wParam)+','+IntToStr(lParam)+')');
  if r=0 then exit(0);
  if r=1 then exit(DefWindowProc(hwnd, Msg, wParam, lParam));

  if assigned(PrevWndFunc) then
    result:=CallWindowProc(PrevWndFunc, hWnd, Msg, wParam, lParam);
end;

end.

