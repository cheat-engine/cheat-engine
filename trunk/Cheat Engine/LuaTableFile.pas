unit LuaTableFile;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lauxlib, lualib;

procedure initializeLuaTableFile;

implementation

uses LuaClass, LuaHandler, LuaObject, MainUnit, luafile;


function findTableFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: string;
  i: integer;

  s: tmemorystream;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(L));
    for i:=0 to mainform.LuaFiles.count-1 do
      if mainform.Luafiles[i].name=f then
      begin
        s:=mainform.Luafiles[i].stream;

        s.position:=0;
        luaclass_newClass(L, mainform.Luafiles[i]); //return the tableFile, not the stream. To get the stream, use  tablefile_getData
        result:=1;
      end;

    if result=0 then //not overriden
    begin
      for i:=0 to mainform.InternalLuaFiles.count-1 do
      begin
        if mainform.InternalLuaFiles[i].name=f then
        begin
          s:=mainform.InternalLuaFiles[i].stream;
          s.position:=0;
          luaclass_newClass(L, mainform.InternalLuaFiles[i]);
          result:=1;
        end;
      end;
    end;

  end
  else
    lua_pop(L, lua_gettop(L));
end;


function tablefile_saveToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  lf: TLuaFile;
  f: string;
  i: integer;
begin
  lf:=luaclass_getClassObject(L);
  if parameters>=1 then
    f:=Lua_ToString(L, -1);

  lf.stream.Position:=0;
  lf.stream.SaveToFile(f);
end;

function tablefile_getData(L: Plua_State): integer; cdecl;
var parameters: integer;
  lf: TLuaFile;
begin
  result:=0;
  lf:=luaclass_getClassObject(L);
  luaclass_newClass(L, lf.stream);
end;

procedure tablefile_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', tablefile_saveToFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getData', tablefile_getData);
end;

procedure initializeLuaTableFile;
begin
  Lua_register(LuaVM, 'findTableFile', findTableFile);
  Lua_register(LuaVM, 'tablefile_saveToFile', tablefile_saveToFile);
  Lua_register(LuaVM, 'tablefile_getData', tablefile_getData);
end;

initialization
  luaclass_register(TLuafile, tablefile_addMetaData);

end.

