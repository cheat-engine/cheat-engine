library vehdebug;

{$mode objfpc}{$H+}

uses
  windows, Classes, init, DebugHandler, VEHDebugSharedMem, threadpoll, extcont,
  SimpleThread
  { you can add units after this };

exports ConfigName;
exports fm;
exports InitializeVEH;
exports UnloadVEH;


begin
  //OutputDebugString('vehdebug loaded. Waiting for init call');
end.

