unit LuaRegion; 

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler, RegionEx, LCLIntf, LCLType;

procedure initializeLuaRegion;

implementation

uses luaobject, luaclass;

function createRegion(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, TRegionEx.Create);
  result:=1;
end;

function region_addRectangle(L: PLua_State): integer; cdecl;
var
  r: TRegion;
  x1,y1,x2,y2: integer;
begin
  result:=0;
  r:=luaclass_getClassObject(L);

  if lua_gettop(L)>=4 then
  begin
    x1:=lua_tointeger(L, -4);
    y1:=lua_tointeger(L, -3);
    x2:=lua_tointeger(L, -2);
    y2:=lua_tointeger(L, -1);
    r.AddRectangle(x1,y1,x2,y2);
  end;
end;

function region_addPolygon(L: PLua_State): integer; cdecl;
var
  paramstart: integer;
  r: TRegionEx;

  coordinatesTable: integer;
  coordinateTable: integer;


  c: array of TPoint;

  tablesize: integer;
  coordinate: integer;


  x,y: integer;

  debug: integer;
begin
  setlength(c,0);
  result:=0;
  r:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    coordinatesTable:=lua_gettop(L);

    if lua_istable(L, coordinatesTable) then
    begin
      tablesize:=lua_objlen(L, coordinatesTable);
      setlength(c, tablesize);


      for coordinate:=1 to tablesize do
      begin
        lua_pushinteger(l,coordinate);
        lua_gettable(L, coordinatesTable);

        coordinateTable:=lua_gettop(L);


        if lua_istable(L, coordinateTable) then
        begin
          //it's a table

          debug:=lua_gettop(L);



          //get x
          lua_pushinteger(l, 1);
          lua_gettable(L, coordinateTable);
          x:=lua_tointeger(L, -1);
          c[coordinate-1].x:=x;
          lua_pop(L, 1);

          //get y
          lua_pushinteger(l, 2);
          lua_gettable(L, coordinateTable);
          y:=lua_tointeger(L, -1);
          c[coordinate-1].y:=y;
          lua_pop(L, 1);
        end else exit;

        lua_pop(L,1);
      end;

      //still here so valid coordinates
      r.AddPolygon(c);
    end;


  end;

  lua_settop(L, 0);
end;

procedure region_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addRectangle', region_addRectangle);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addPolygon', region_addPolygon);

end;

procedure initializeLuaRegion;
begin
  lua_register(LuaVM, 'createRegion', createRegion);
  lua_register(LuaVM, 'region_addRectangle', region_addRectangle);
  lua_register(LuaVM, 'region_addPolygon', region_addPolygon);
end;

initialization
  luaclass_register(TRegion, region_addMetaData);


end.

