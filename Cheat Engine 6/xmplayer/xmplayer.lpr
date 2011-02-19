program xmplayer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, main, xmplayer_defines
  { you can add units after this };

begin
  StartListening;
end.

