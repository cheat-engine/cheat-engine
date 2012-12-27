unit LuaObject;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, math;

procedure InitializeObject;
procedure object_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaClass, LuaHandler, pluginexports;

function object_destroy(L: PLua_State): integer; cdecl;
var c: TObject;
  metatable: integer;
  i: integer;
begin
  i:=ifthen(lua_type(L, lua_upvalueindex(1))=LUA_TUSERDATA, lua_upvalueindex(1), 1);
  c:=pointer(lua_touserdata(L, i)^);
  lua_getmetatable(L, i);
  metatable:=lua_gettop(L);

  try
    c.free;
  except
  end;

  if lua_type(L, metatable)=LUA_TTABLE then
  begin
    lua_pushstring(L, '__autodestroy');
    lua_pushboolean(L, false); //make it so it doesn't need to be destroyed (again)
    lua_settable(L, metatable);
  end;
end;

function object_getClassName(L: PLua_state): integer; cdecl;
var c: TObject;
begin
  c:=pointer(lua_touserdata(L, ifthen(lua_type(L, lua_upvalueindex(1))=LUA_TUSERDATA, lua_upvalueindex(1), 1))^);

  lua_pushstring(L, c.ClassName);
  result:=1;
end;

procedure object_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //no parent class metadata to add
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getClassName', object_getClassName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'destroy', object_destroy);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ClassName', object_getClassName, nil);
end;

procedure InitializeObject;
begin
  lua_register(LuaVM, 'object_getClassName', object_getClassName);
  lua_register(LuaVM, 'object_destroy', object_destroy);
end;

end.

