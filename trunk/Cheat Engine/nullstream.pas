unit NullStream;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type TNullStream=class(TStream)
  private
  public
    function Write(const Buffer; Count: Longint): Longint; override;
end;


implementation

function TNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  result:=count;
end;


end.

