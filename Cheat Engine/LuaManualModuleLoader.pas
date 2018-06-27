unit LuaManualModuleLoader;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure initializeLuaModuleLoader;

implementation

uses ManualModuleLoader, lua, lauxlib, lualib, LuaClass, LuaHandler, LuaObject;

function moduleloader_createModuleLoader(L: PLua_State): integer; cdecl;
var
  ml: TModuleLoader;
  filename: string;
begin
  result:=0;
  if lua_gettop(L)>1 then
  begin
    filename:=Lua_ToString(L,1);
    try
      ml:=TModuleLoader.create(filename);
      ml.createSymbolListHandler;
    except
      exit;
    end;

    luaclass_newClass(L,ml);
    result:=1;
  end;
end;

function moduleloader_getExports(L: PLua_State): integer; cdecl;
var
  ml: TModuleLoader;
  i: integer;
begin
  ml:=luaclass_getClassObject(L);
  lua_newtable(L);
  for i:=0 to ml.Exporttable.Count-1 do
  begin
    lua_pushstring(L, ml.Exporttable[i]);
    lua_pushinteger(L, ptruint(ml.Exporttable.objects[i]));
    lua_settable(L,-3);
  end;
  result:=1;
end;

procedure moduleloader_addMetadata(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Exports', moduleloader_getExports, nil);
end;

procedure initializeLuaModuleLoader;
begin
  lua_register(LuaVM, 'loadModule', moduleloader_createModuleLoader);

end;


initialization
  luaclass_register(TModuleLoader, moduleloader_addMetadata);

end.

