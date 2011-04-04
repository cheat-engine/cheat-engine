unit LuaPicture;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaPicture;

implementation

function createPicture(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));

  lua_pushlightuserdata(L, TPicture.Create);
  result:=1;
end;

function picture_loadFromFile(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  picture: Tpicture;
  filename: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    picture:=lua_touserdata(L,-parameters);
    filename:=Lua_ToString(L, -parameters+1);
    lua_pop(L, parameters);

    picture.LoadFromFile(filename);
  end else lua_pop(L, parameters);
end;

function picture_loadFromStream(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  picture: Tpicture;
  stream: TStream;
  ext: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    picture:=lua_touserdata(L,-parameters);
    stream:=lua_touserdata(L, -parameters+1);

    if parameters=3 then
      ext:=Lua_ToString(L, -parameters+2);

    lua_pop(L, parameters);

    if parameters=2 then
      picture.LoadFromStream(stream)
    else
      picture.LoadFromStreamWithFileExt(stream,ext);

  end else lua_pop(L, parameters);
end;

function picture_assign(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  picture, picture2: Tpicture;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    picture:=lua_touserdata(L,-parameters);
    picture2:=lua_touserdata(L, -parameters+1);
    lua_pop(L, parameters);

    picture.Assign(picture2);
  end else lua_pop(L, parameters);
end;

function picture_getGraphic(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  picture, picture2: Tpicture;
  g: TGraphic;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    picture:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    g:=picture.Graphic;

    result:=1;
    lua_pushlightuserdata(L, g);
  end else lua_pop(L, parameters);
end;

procedure initializeLuaPicture;
var x: TPicture;
begin
  lua_register(LuaVM, 'createPicture', createPicture);
  lua_register(LuaVM, 'picture_loadFromFile',picture_loadFromFile);
  lua_register(LuaVM, 'picture_loadFromStream',picture_loadFromStream);
  lua_register(LuaVM, 'picture_assign',picture_assign);
  lua_register(LuaVM, 'picture_getGraphic',picture_getGraphic);

end;

end.

