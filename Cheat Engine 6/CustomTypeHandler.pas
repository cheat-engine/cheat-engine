unit CustomTypeHandler;

{$mode delphi}
{
This class is used as a wrapper for different kinds of custom types
}

interface

uses
  Classes, SysUtils,cefuncproc, autoassembler;

type TConversionRoutine=function(data: pointer):integer; stdcall;
type TReverseConversionRoutine=procedure(i: integer; output: pointer); stdcall;

type TCustomType=class
  private
    routine: TConversionRoutine;
    reverseroutine: TReverseConversionRoutine;

    assignedfromscript: boolean;
    c: TCEAllocArray;
    currentscript: tstringlist;

  public
    name: string;
    bytesize: integer;
    function ConvertDataToInteger(data: pointer): integer;
    procedure ConvertIntegerToData(i: integer; output: pointer);

    constructor CreateTypeFromAutoAssemblerScript(script: tstrings);
    destructor destroy; override;
end;

var customTypes: TList; //list holding all the custom types

implementation

procedure TCustomType.ConvertIntegerToData(i: integer; output: pointer);
begin
  if assigned(reverseroutine) then
    reverseroutine(i,output);
end;

function TCustomType.ConvertDataToInteger(data: pointer): integer;
begin
  if assigned(routine) then
    result:=routine(data);
end;

constructor TCustomType.CreateTypeFromAutoAssemblerScript(script: tstrings);
var i,j: integer;
  found: boolean;
begin
  inherited create;

  setlength(c,0);
  if autoassemble(script,false, true, false, true, c) then
  begin
    //find alloc "ConvertRoutine"
    for i:=0 to length(c)-1 do
    begin
      if uppercase(c[i].varname)='CONVERTROUTINE' then
        routine:=pointer(c[i].address);

      if uppercase(c[i].varname)='BYTESIZE' then
        bytesize:=pinteger(c[i].address)^;

      if uppercase(c[i].varname)='TYPENAME' then
        name:=pchar(c[i].address);

      if uppercase(c[i].varname)='CONVERTBACKROUTINE' then
        reverseroutine:=pointer(c[i].address);

    end;

    assignedfromscript:=true;
    currentscript:=tstringlist.create;
    script.Assign(currentscript);


    //check if there is already a script with this name
    found:=false;
    for i:=0 to customtypes.count-1 do
      if TCustomType(customtypes[i]).name=name then
      begin
        //already exists. Overwrite the old one
        TCustomType(customtypes[i]).bytesize:=bytesize;
        TCustomType(customtypes[i]).routine:=routine;
        TCustomType(customtypes[i]).reverseroutine:=reverseroutine;
        TCustomType(customtypes[i]).currentscript.text:=currentscript.text;

        setlength(TCustomType(customtypes[i]).c,length(c));
        for j:=0 to length(c)-1 do
          TCustomType(customtypes[i]).c[j]:=c[j];

        found:=true;
      end;

    if not found then customtypes.Add(self); //no duplicate, add it
  end;
end;

destructor TCustomType.destroy;
var i: integer;
begin
  if assignedfromscript then
  begin
    if currentscript<>nil then
    begin
      autoassemble(currentscript,false, false, false, true, c); //popupmessages is false so it won't complain if there is no disable section
      currentscript.free;
    end;
  end;

  //remove self from array
  i:=customTypes.IndexOf(self);
  if i<>-1 then
    customTypes.Delete(i);

  inherited destroy;
end;

initialization
  customTypes:=Tlist.create;

finalization
  if customTypes<>nil then
    customtypes.free;

end.

