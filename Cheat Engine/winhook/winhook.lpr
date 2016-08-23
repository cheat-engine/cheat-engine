library winhook;

{
Just a silly experiment using the lua interpreter on CE's side for hooking window handlers
}
{$mode objfpc}{$H+}

uses
  Classes, com, proc
  { you can add units after this };

{$R *.res}

begin
  TServer.Create(false);
  CEConnection:=TCEConnection.Create();
end.

