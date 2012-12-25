unit LuaClass;

//Handles some of the common used class code


{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib;


function luaclass_createMetaTable(L: Plua_State): integer;

procedure luaclass_addClassFunctionToTable(L: PLua_State; table: integer; o: TObject; functionname: string; f: lua_CFunction);
procedure luaclass_addPropertyToTable(L: PLua_State; table: integer; o: TObject; propertyname: string; getfunction: lua_CFunction; setfunction: lua_CFunction);

implementation

procedure luaclass_addPropertyToTable(L: PLua_State; table: integer; o: TObject; propertyname: string; getfunction: lua_CFunction; setfunction: lua_CFunction);
var t: integer;
begin
  lua_pushstring(L, propertyname);
  lua_newtable(L);
  t:=lua_gettop(L);

  lua_pushstring(L,'__get');
  lua_pushlightuserdata(L, o);
  lua_pushcclosure(L, getfunction, 1);
  lua_settable(L, t);

  lua_pushstring(L,'__set');
  lua_pushlightuserdata(L, o);
  lua_pushcclosure(L, setfunction, 1);
  lua_settable(L, t);

  lua_settable(L, table);
end;

procedure luaclass_addClassFunctionToTable(L: PLua_State; table: integer; o: TObject; functionname: string; f: lua_CFunction);
begin
  lua_pushstring(L, functionname);
  lua_pushlightuserdata(L, o);
  lua_pushcclosure(L, f, 1);
  lua_settable(L, table);
end;

function luaclass_newindex(L: PLua_State): integer; cdecl; //set
//parameters: (self, key, newvalue)
begin
  lua_getmetatable(L, 1); //get the metatable of self
  lua_pushvalue(L, 2); //push the key

  lua_gettable(L, -2); //metatable[key]
  if lua_istable(L ,-1) then
  begin
    lua_pushstring(L, '__set');
    lua_gettable(L, -2); //table['__set']

    if lua_isfunction(L, -1) then
    begin
      lua_pushvalue(L, 3); //push newvalue    (so stack now holds, function, newvalue)
      lua_call(L, 1, 0);
    end;
  end;
end;

function luaclass_index(L: PLua_State): integer; cdecl; //get
//parameters: (self, key)
//called when a class object is indexed
//return the metatable element with this name

//wants to get the value of table[key] , but table isn'ty really a table
var i: integer;

    metatable: integer;

    s: string;
begin
  //return the metatable element


  i:=lua_gettop(L);

  result:=0;

  if i=2 then
  begin
    lua_getmetatable(L, 1); //get the metatable from the table and push i on the stack
    metatable:=lua_gettop(L);

    lua_pushvalue(L, 2); //push the key on the stack
    //lua_rawget(L, metatable);
    lua_gettable(L, metatable); //get metatable[key]

    if lua_istable(L ,-1) then
    begin
      //lua_getmetatable(L,-1);
      lua_pushstring(L, '__get');
      lua_gettable(L, -2);

      if lua_isfunction(L, -1) then
        lua_call(L, 0, 1);
    end;
    result:=1;
  end;

end;


function luaclass_createMetaTable(L: Plua_State): integer;
//creates a table to be used as a metatable
//returns the stack index of the table
begin
  lua_newtable(L);
  result:=lua_gettop(L);

  //set the index method
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, luaclass_index);
  lua_settable(L, result);

  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, luaclass_newindex);
  lua_settable(L, result);
end;


end.

