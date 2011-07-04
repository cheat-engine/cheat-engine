program standalonephase2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, main
  { you can add units after this };

{$ifdef cpu64}
  {$ifdef release}
    {$error The launcher should be build for 32-bit only}
  {$endif}
{$endif}

{$R ..\..\manifest.res}

begin
  Launch;
end.

