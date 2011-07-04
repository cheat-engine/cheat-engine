unit tp_register;

{$mode objfpc}

interface

uses
  Classes, SysUtils; 

procedure register;


implementation
uses
  jvDesignSurface, LResources;

procedure Register;
begin
  RegisterComponents('Jv Runtime Design', [TJvDesignSurface, TJvDesignScrollBox, TJvDesignPanel]);
end;

initialization
  {$I test.lrs}

end.

