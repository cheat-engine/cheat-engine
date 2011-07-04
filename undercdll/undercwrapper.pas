unit undercwrapper;

interface

uses underc,windows;

function underc_executescript(script: pchar):bool; stdcall;
function underc_geterror: pchar; stdcall;

implementation

var lasterror: string;

function underc_executescript(script: pchar):bool;
{
This function will use the underc engine to execute the given script
}
var s: string;
begin
  if scriptengine.beginScript then
  begin
    try
      s:=script;
      result:=scriptengine.execute_command(s);

      lasterror:=scriptengine.getError;
    finally
      scriptengine.endScript;
    end;
  end
  else result:=false; //only possible if the user fucked with the dll himself
end;

function underc_geterror: pchar;
begin
  result:=@lasterror[1];
end;

end.
