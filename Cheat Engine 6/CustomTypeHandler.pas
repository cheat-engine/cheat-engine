unit CustomTypeHandler;

{$mode delphi}
{
This class is used as a wrapper for different kinds of custom types
}

interface

uses
  Classes, SysUtils,cefuncproc, autoassembler;

type TConversionRoutine=function(data: pointer):integer;

type TCustomType=class
  private
    routine: TConversionRoutine;

    assignedfromscript: boolean;
    c: TCEAllocArray;
    currentscript: tstringlist;

  public
    name: string;
    function ConvertDataToInteger(data: pointer): integer;
    procedure assignRoutine(r: pointer);
    procedure assignRoutineFromAutoAssemblerScript(script: tstrings);
    destructor destroy; override;
end;

var customTypes: array of TCustomType; //array holding all the custom types

implementation

function TCustomType.ConvertDataToInteger(data: pointer): integer;
begin
  if assigned(routine) then
    result:=routine(data);
end;

procedure TCustomType.assignRoutine(r: pointer);
begin
  routine:=r;
end;

procedure TCustomType.assignRoutineFromAutoAssemblerScript(script: tstrings);
var i: integer;
begin
  setlength(c,0);
  if autoassemble(script,true, true, false, true, c) then
  begin
    //find alloc "ConvertRoutine"
    for i:=0 to length(c)-1 do
      if uppercase(c[i].varname)='CONVERTROUTINE' then
      begin
        assignedfromscript:=true;

        currentscript:=tstringlist.create;
        script.Assign(currentscript);

        assignRoutine(pointer(c[i].address));
      end;
  end;
end;

destructor TCustomType.destroy;
begin
  if assignedfromscript then
  begin
    if currentscript<>nil then
    begin
      autoassemble(currentscript,true, false, false, true, c);
      currentscript.free;
    end;
  end;
  inherited destroy;
end;

end.

