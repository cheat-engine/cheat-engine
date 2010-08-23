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


type
  TCustomTypeType=(cttAutoAssembler, cttPlugin);
  TCustomType=class
  private
    routine: TConversionRoutine;
    reverseroutine: TReverseConversionRoutine;

    c: TCEAllocArray;
    currentscript: tstringlist;
    fCustomTypeType: TCustomTypeType; //plugins set this to cttPlugin
    procedure unloadscript;

  public
    name: string;
    bytesize: integer;

    function ConvertDataToInteger(data: pointer): integer;
    procedure ConvertIntegerToData(i: integer; output: pointer);

    function getScript:string;
    procedure setScript(script:string);
    constructor CreateTypeFromAutoAssemblerScript(script: string);
    destructor destroy; override;

    procedure remove;  //call this instead of destroy

    property CustomTypeType: TCustomTypeType read fCustomTypeType;
    property script: string read getScript write setScript;
end;

function GetCustomTypeFromName(name:string):TCustomType; //global function to retrieve a custom type

var customTypes: TList; //list holding all the custom types

implementation

function GetCustomTypeFromName(name:string): TCustomType;
var i: integer;
begin
  result:=nil;
  for i:=0 to customTypes.Count-1 do
    if uppercase(TCustomType(customtypes[i]).name)=name then
    begin
      result:=TCustomType(customtypes[i]);
      break;
    end;
end;

function TCustomType.getScript: string;
begin
  if (fCustomTypeType=cttAutoAssembler) and (currentscript<>nil) then
    result:=currentscript.text
  else
    result:='';
end;

procedure TCustomType.ConvertIntegerToData(i: integer; output: pointer);
begin
  if assigned(reverseroutine) then
    reverseroutine(i,output);
end;

function TCustomType.ConvertDataToInteger(data: pointer): integer;
begin
  if assigned(routine) then
    result:=routine(data)
  else
    result:=0;
end;

procedure TCustomType.unloadscript;
begin
  if fCustomTypeType=cttAutoAssembler then
  begin
    routine:=nil;
    reverseroutine:=nil;

    if currentscript<>nil then
    begin
      autoassemble(currentscript,false, false, false, true, c); //popupmessages is false so it won't complain if there is no disable section
      currentscript.free;
    end;
  end;
end;

procedure TCustomType.setScript(script:string);
var i: integer;
  s: tstringlist;
begin
  unloadscript;



  setlength(c,0);
  s:=tstringlist.create;
  try
    s.text:=script;

    if autoassemble(s,false, true, false, true, c) then
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

      fCustomTypeType:=cttAutoAssembler;
      currentscript:=tstringlist.create;
      currentscript.text:=script;

      //check if there is already a script with this name (and not this one)
      for i:=0 to customtypes.count-1 do
        if uppercase(TCustomType(customtypes[i]).name)=uppercase(name) then
        begin
          if TCustomType(customtypes[i])<>self then
            raise exception.create('A custom type with name '+name+' already exists');
        end;

    end;

  finally
    s.free;
  end;

end;

constructor TCustomType.CreateTypeFromAutoAssemblerScript(script: string);
begin
  inherited create;

  setScript(script);

  //still here so everything ok
  customtypes.Add(self);

end;

procedure TCustomType.remove;
var i: integer;
begin
  unloadscript;

  //remove self from array
  i:=customTypes.IndexOf(self);
  if i<>-1 then
    customTypes.Delete(i);

end;

destructor TCustomType.destroy;
begin
  remove;
end;

initialization
  customTypes:=Tlist.create;

finalization
  if customTypes<>nil then
    customtypes.free;

end.

