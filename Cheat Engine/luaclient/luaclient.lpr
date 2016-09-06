library luaclient;

{$mode objfpc}{$H+}

uses
  Classes, luaclientfunctions
  { you can add units after this };

exports CELUA_Initialize;
exports CELUA_ExecuteFunction;
exports CELUA_ExecuteFunctionAsync;
exports CELUA_GetFunctionReferenceFromName;
exports CELUA_ExecuteFunctionByReference;
exports CELUA_ServerName;

begin
end.

