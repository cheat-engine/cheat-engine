unit LuaClassArray;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib;

function luaclassarray_createMetaTable(L: Plua_State; userdata: integer; getArrayFunction: lua_CFunction; setArrayFunction:lua_CFunction=nil ): integer;

implementation

function luaclassarray_newindex(L: PLua_State): integer; cdecl; //set
//parameters: (self, key, value)
var
  s: string;
  cf: lua_CFunction;
begin
  s:=lua_tostring(L, 2);
  if s='__set' then
  begin
    lua_pushvalue(L, 1);
    exit(1);
  end;

  cf:=lua_tocfunction(L, lua_upvalueindex(2));
  lua_pushvalue(L, lua_upvalueindex(1)); //push the object
  lua_pushcclosure(L, cf, 1); //create a cclosure function on the stack
  lua_pushvalue(L, 2); //push the key
  lua_pushvalue(L, 3); //push the value
  lua_call(L,2,0); //call setArrayFunction(key, value) : object
  result:=1;

end;

function luaclassarray_index(L: PLua_State): integer; cdecl; //get
//parameters: (self, key)
//called when an array object in a class is being indexed
var s: string;

  cf: lua_CFunction;
begin
  s:=lua_tostring(L, 2);
  if s='__get' then
  begin
    //just return the table itself
    lua_pushvalue(L, 1);
    exit(1);
  end;


  //not __get, so get the value at key
  cf:=lua_tocfunction(L, lua_upvalueindex(2));
  lua_pushvalue(L, lua_upvalueindex(1)); //push the object
  lua_pushcclosure(L, cf, 1); //create a cclosure function on the stack
  lua_pushvalue(L, 2); //push the key
  lua_call(L,1,1); //call getArrayFunction(key) : object
  result:=1;
end;

function luaclassarray_createMetaTable(L: Plua_State; userdata: integer; getArrayFunction: lua_CFunction; setArrayFunction:lua_CFunction=nil ): integer;
begin
  lua_newtable(L);
  result:=lua_gettop(L);

  //set the index method
  lua_pushstring(L, '__index');
  lua_pushvalue(L, userdata);
  lua_pushcfunction(L, getArrayFunction);
  lua_pushcclosure(L, luaclassarray_index, 2);
  lua_settable(L, result);

  if assigned(setArrayFunction) then
  begin
    lua_pushstring(L, '__newindex');
    lua_pushvalue(L, userdata);
    lua_pushcfunction(L, setArrayFunction);
    lua_pushcclosure(L, luaclassarray_newindex, 2);
    lua_settable(L, result);
  end;
end;

end.

