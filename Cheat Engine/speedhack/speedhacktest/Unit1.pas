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
    oldtick: dword;
    oldperf: int64;
  public
    { Public declarations }
  end;

type TTimeGetTime=function: DWORD; stdcall;

var
  Form1: TForm1;
  timeGetTime: TTimeGetTime;


implementation


procedure TForm1.Timer1Timer(Sender: TObject);
var freq: int64;


  newtick: dword;
  newperf: int64;
begin
  newtick:=gettickcount;
  label1.Caption:=inttostr(gettickcount div 1000);
  if assigned(timegettime) then
    label2.Caption:=inttostr(timegettime div 1000);

  QueryPerformanceCounter(newperf);
  QueryPerformanceFrequency(freq);
  label3.Caption:=inttostr(newperf)+' = '+inttostr(newperf div freq);


  try
    if newtick<oldtick then
      raise exception.create('Speedhack fail. GetTickCount');

    if newperf<oldperf then
      raise exception.create('Speedhack fail. QueryPerformanceCounter');
  except
    on e:exception do
    begin
      timer1.enabled:=false;
      raise e.create(e.Message);
    end;
  end;

  oldperf:=newperf;
  oldtick:=newtick;

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
