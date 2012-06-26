unit frmDriverLoadedUnit;
//just something funny

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmDriverLoaded }

  TfrmDriverLoaded = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure ShowDriverLoaded;

implementation

{$R *.lfm}

uses mainunit, pluginexports;


procedure ShowDriverLoaded;
var
  frmDriverLoaded: TfrmDriverLoaded;
  s,s2,t: string;
  i: integer;
  h: hwnd;

  a,b: array [0..1] of boolean;


begin
//  TfrmDriverLoaded.ClassName;
  s:='';
  s2:='';
  for i:=1 to 5+random(5) do
  begin
    s:=s+chr(31+random(96));
    s2:=s2+chr(31+random(96));
  end;

  a[0]:=true;
  a[1]:=false;
  for i:=0 to random(10) do
  begin
    a[0]:=not a[0];
    a[1]:=not a[1];
    t:=s;
    s:=s2;
    s2:=t;
  end;

  frmDriverLoaded:=TfrmDriverLoaded.Create(nil);
  frmDriverLoaded.caption:=s;

  frmDriverLoaded.show;


  b[0]:=IsWindowVisible(FindWindow('Window',pchar(s)));
  b[1]:=IsWindowVisible(FindWindow('Window',pchar(s2)));

 // b[1]:=b[0]; (debug)

  if (a[0]<>b[0]) or (a[1]<>b[1]) or (a[0]=a[1]) or (b[0]=b[1]) then
  begin
    TerminateProcess(GetCurrentProcess,0);
    MainForm.free;
    application.Terminate;
    while true do ;
  end;

  frmDriverLoaded.timer1.Enabled:=true;

end;

procedure TfrmDriverLoaded.Timer1Timer(Sender: TObject);
var s: integer;
begin
  s:=AlphaBlendValue-2;
  AlphaBlendValue:=s;
 { if s<=0 then
    close;  }
end;

procedure TfrmDriverLoaded.Timer2Timer(Sender: TObject);
begin
  color:=random($ffffff);
end;

end.

