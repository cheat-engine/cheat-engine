unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, Classes, windows, Messages, SysUtils, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type
  TSpeedhackTest=class(tthread)
  public
    procedure execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Label4: TLabel;
    lblFail: TLabel;
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

  fail: boolean;


implementation

procedure TSpeedhackTest.execute;
var
  oldtick, newtick: dword;
  oldperf, newperf: int64;

begin
  oldtick:=gettickcount;
  QueryPerformanceCounter(oldperf);

  while (not fail) and (not terminated) do
  begin
    newtick:=gettickcount;
    QueryPerformanceCounter(newperf);

    if newtick<oldtick then
      fail:=true;

    if newperf<oldperf then
      fail:=true;


  {  if random(100000)=66 then
      fail:=true; }


    oldperf:=newperf;
    oldtick:=newtick;
  end;

end;

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

  label4.caption:=inttostr(gettickcount64 div 1000);

  if fail then
      lblFail.visible:=true;


end;



procedure TForm1.FormCreate(Sender: TObject);
var xx: HModule;
  i: integer;
begin
  xx:=loadlibrary('winmm.dll');
  timeGetTime:=GetProcAddress(xx,'timeGetTime');

  for i:=0 to 2 do
    TSpeedhackTest.create(false);


end;

initialization
  {$i Unit1.lrs}

end.
