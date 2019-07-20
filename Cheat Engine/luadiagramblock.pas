unit LuaDiagramBlock;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib,LuaHandler, diagram, diagramblock,
  diagramlink, diagramtypes, typinfo;

implementation

uses controls, luaclass, LuaCustomControl, LuaObject, LuaDiagram;


function diagramBlock_intersectsWithLine(L: PLua_state): integer; cdecl;
var
  b: TDiagramBlock;
  p1: tpoint;
  p2: tpoint;
  r: tpoint;
begin
  result:=0;
  b:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    if lua_istable(L,1) and lua_istable(L,2) then
    begin
      p1:=lua_toPoint(L,1);
      p2:=lua_toPoint(L,2);

      if b.IntersectsWithLine(p1,p2,r) then
      begin
        lua_pushboolean(L,true);
        lua_pushpoint(L,r);
      end
      else
      begin
        lua_pushboolean(L,false);
        result:=1;
      end;
    end;
  end;
end;

function diagramBlock_overlapsWith(L: PLua_state): integer; cdecl;
var
  b1,b2: TDiagramBlock;
  r: boolean;
begin
  b1:=luaclass_getClassObject(L);
  if lua_Gettop(L)>=1 then
  begin
    b2:=lua_ToCEUserData(L,1);
    r:=b1.OverlapsWith(b2);
    lua_pushboolean(L, r);
    result:=1;
  end
  else
    result:=0;
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
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'intersectsWithLine', diagramBlock_intersectsWithLine);

end;

initialization
  luaclass_register(TDiagramBlock, diagramBlock_addMetaData);

end.

