library winhook;

{
Just a silly experiment using the lua interpreter on CE's side for hooking window handlers
}
{$mode objfpc}{$H+}

uses
  windows, Classes, com, proc
  { you can add units after this };

{$R *.res}

var
  s: TServer;
  tid: dword;
  cnt: integer;

  ct: TThread;
begin
  CreateThread(nil,0,@execute2, nil,0,tid); //this works in 32-bit
  //TServer.Create(false); //this doesn't
  CEConnection:=TCEConnection.Create();
end.

