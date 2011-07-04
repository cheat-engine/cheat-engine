unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Classes, windows, Messages, SysUtils, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type TTimeGetTime=function: DWORD; stdcall;

var
  Form1: TForm1;
  timeGetTime: TTimeGetTime;


implementation


procedure TForm1.Timer1Timer(Sender: TObject);
var x,freq: int64;
begin
  label1.Caption:=inttostr(gettickcount div 1000);
  if assigned(timegettime) then
    label2.Caption:=inttostr(timegettime div 1000);

  QueryPerformanceCounter(x);
  QueryPerformanceFrequency(freq);
  label3.Caption:=inttostr(x)+' = '+inttostr(x div freq);
end;



procedure TForm1.FormCreate(Sender: TObject);
var xx: HModule;
begin
  xx:=loadlibrary('winmm.dll');
  timeGetTime:=GetProcAddress(xx,'timeGetTime');
end;

initialization
  {$i Unit1.lrs}

end.
