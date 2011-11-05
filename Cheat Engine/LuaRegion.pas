unit LuaRegion; 

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler, RegionEx;

procedure initializeLuaRegion;

implementation

function createRegion(L: Plua_State): integer; cdecl;
var
  r: TRegionEx;
  parameters: integer;
begin
  result:=1;
  lua_pop(L, lua_gettop(L));

  r:=TRegionEx.Create;
  lua_pushlightuserdata(L, r);
  result:=1;
end;

function region_addRectangle(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  r: TRegion;
  x1,y1,x2,y2: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=5 then
  begin
    r:=lua_touserdata(L,-parameters);
    x1:=lua_tointeger(L, -parameters+1);
    y1:=lua_tointeger(L, -parameters+2);
    x2:=lua_tointeger(L, -parameters+3);
    y2:=lua_tointeger(L, -parameters+4);
    lua_pop(L, parameters);

    r.AddRectangle(x1,y1,x2,y2);
  end else lua_pop(L, parameters);
end;

function region_addPolygon(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  r: TRegionEx;

  coordinatesTable: integer;
  coordinateTable: integer;


  c: array of POINT;

  tablesize: integer;
  coordinate: integer;


  x,y: integer;
begin
  setlength(c,0);
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    r:=lua_touserdata(L,1);
    coordinatesTable:= 2;

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

          //get x
          lua_pushinteger(l, 1);
          lua_gettable(L, coordinateTable);
          c[coordinate-1].x:=lua_tointeger(L, -1);
          lua_pop(L, 1);

          //get y
          lua_pushinteger(l, 2);
          lua_gettable(L, coordinateTable);
          c[coordinate-1].y:=lua_tointeger(L, -1);
          lua_pop(L, 1);
        end else exit;
      end;

      //still here so valid coordinates
      r.AddPolygon(c);
    end;


  end;

  lua_settop(L, 0);
end;


procedure initializeLuaRegion;
begin
  lua_register(LuaVM, 'createRegion', createRegion);
  lua_register(LuaVM, 'region_addRectangle', region_addRectangle);
  lua_register(LuaVM, 'region_addPolygon', region_addPolygon);
end;

end.

