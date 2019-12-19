unit LuaHeaderSections;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, cefuncproc, commonTypeDefs;

implementation

uses luaclass, LuaCollection, comctrls;


function headersections_add(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, THeaderSections(luaclass_getClassObject(L)).add);
  result:=1;
end;

function headersections_insert(L: PLua_State): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
  begin
    luaclass_newClass(L, THeaderSections(luaclass_getClassObject(L)).insert(lua_tointeger(L,1)));
    result:=1;
  end
  else
    result:=0;
end;

function headersections_delete(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    THeaderSections(luaclass_getClassObject(L)).delete(lua_tointeger(L,1));
end;

function headersections_getItem(L: PLua_State): integer; cdecl;
var
  hs: THeaderSections;
  index: integer;
begin
  result:=0;
  hs:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,1);
    luaclass_newClass(L, hs.Items[index]);
    result:=1;
  end;
end;

function headersections_setItem(L: PLua_State): integer; cdecl;
var
  hs: THeaderSections;
  s: THeaderSection;
  index: integer;
begin
  result:=0;
  hs:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    index:=lua_tointeger(L,1);
    s:=lua_ToCEUserData(L,2);

    hs.Items[index]:=s;
  end;
end;

procedure headersections_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  collection_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', headersections_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'insert', headersections_insert);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', headersections_delete);


  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Items', headersections_getItem, headersections_setItem);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, headersections_getItem, headersections_setItem );
end;


initialization
  luaclass_register(THeaderSections, headersections_addMetaData);

end.

