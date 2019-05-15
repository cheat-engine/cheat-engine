unit LuaDiagramBlock;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib,LuaHandler, diagram, diagramblock,
  diagramlink, diagramtypes, typinfo;

implementation

uses controls, luaclass, LuaCustomControl, LuaObject, LuaDiagram;


function diagramBlock_overlapsWith(L: PLua_state): integer; cdecl;
var
  b1,b2: TDiagramBlock;
  r: boolean;
begin
  b1:=luaclass_getClassObject(L);
  if lua_Gettop(L)>=1 then
    b2:=lua_ToCEUserData(L,1);

  r:=b1.OverlapsWith(b2);
  lua_pushboolean(L, r);
  result:=1;
end;

function diagramBlock_getLinks(L: PLua_state): integer; cdecl;
var
  db: TDiagramBlock;
  d: TDiagram;
  List: TList;
  t: integer;
  t2: integer;
  i: integer;
begin
  db:=luaclass_getClassObject(L);
  d:=TDiagram(db.Owner);

  lua_newtable(L);
  t:=lua_gettop(l);

  List:=tlist.create;
  d.getConnectionsFromBlock(db,List);
  lua_pushstring(L, 'asSource');
  lua_newtable(L);
  t2:=lua_gettop(L);

  for i:=0 to list.count-1 do
  begin
    lua_pushinteger(L,i+1);
    luaclass_newClass(L,list[i]);
    lua_settable(L,t2);
  end;
  lua_settable(L,t);

  List.Clear;
  d.getConnectionsToBlock(db,List);
  lua_pushstring(L, 'asDestination');
  lua_newtable(L);
  t2:=lua_gettop(L);

  for i:=0 to list.count-1 do
  begin
    lua_pushinteger(L,i+1);
    luaclass_newClass(L,list[i]);
    lua_settable(L,t2);
  end;
  lua_settable(L,t);

  list.free;

  result:=1;
end;

procedure diagramBlock_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getLinks', diagramBlock_getLinks);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'overlapsWith', diagramBlock_overlapsWith);

end;

initialization
  luaclass_register(TDiagramBlock, diagramBlock_addMetaData);

end.

