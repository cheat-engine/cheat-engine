unit first;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses windows;

procedure setDPIAware;
type
  PROCESS_DPI_AWARENESS=(PROCESS_DPI_UNAWARE=0, PROCESS_SYSTEM_DPI_AWARE=1, PROCESS_PER_MONITOR_DPI_AWARE=2);

var
  SetProcessDpiAwareness:function(value: PROCESS_DPI_AWARENESS):HRESULT; stdcall;
  SetProcessDPIAware:function: BOOL; stdcall;
  l: HModule;


  Metrics: TNonClientMetrics;
begin
  l:=LoadLibrary('Shcore.dll');
  if l<>0 then
  begin
    farproc(SetProcessDpiAwareness):=GetProcAddress(l,'SetProcessDpiAwareness');

    if assigned(SetProcessDpiAwareness) then
    begin
      SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE);
      exit;
    end;
  end;

  //still here, probably win8.0 or 7
  l:=LoadLibrary('user32.dll');
  if l<>0 then
  begin
    farproc(SetProcessDPIAware):=GetProcAddress(l,'SetProcessDPIAware');
    if assigned(SetProcessDPIAware) then
      SetProcessDPIAware;
  end;

  Metrics.cbSize := SizeOf(Metrics);
  Windows.SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(Metrics), @Metrics, 0);

  Metrics.lfStatusFont.lfHeight:=20;

  Windows.SystemParametersInfo(SPI_SETNONCLIENTMETRICS, SizeOf(Metrics), @Metrics, 0);

end;

var i: integer;
initialization
  for i:=1 to Paramcount do
    if ParamStr(i)='DPIAWARE' then
    begin
      setDPIAware;
      break;
    end;



end.

