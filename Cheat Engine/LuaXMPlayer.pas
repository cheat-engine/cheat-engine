unit LuaXMPlayer;
//special case, the class methods don't really care if the class is provided
{$mode delphi}

interface

uses
  Classes, SysUtils, xmplayer_defines, xmplayer_server, lua, lauxlib, lualib, luafile, LuaHandler;

procedure initializeLuaXMPlayer;

implementation

uses luaclass, LuaObject;

function xmplayer_playXM(L: Plua_State): integer; cdecl;
var parameters: integer;
  lf: TLuaFile;
  f: string;
  i: integer;
  o: TObject;
  s: TStream;
  noloop: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if (xmplayer<>nil) and (parameters>=1) then
  begin
    if parameters>=2 then
      noloop:=lua_toboolean(L,2)
    else
      noloop:=false;

    if lua_isuserdata(L,1) then //Object, if the given object is a LuaFile, get the stream. If it's a stream, use it as it is
    begin
      o:=lua_toceuserdata(L,1);
      if o is TStream then
        s:=TStream(o)
      else
      if o is TLuafile then
        s:=TLuafile(o).stream
      else
        exit;

      xmplayer.playXM(s, noloop)
    end
    else
      xmplayer.playXM(Lua_ToString(L,1), noloop)
  end;

  lua_pop(L, lua_gettop(L));
end;

function xmplayer_pause(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
     xmplayer.pause;
end;

function xmplayer_resume(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
     xmplayer.resume;
end;

function xmplayer_stop(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if (xmplayer<>nil) then
    xmplayer.stop();
end;

function xmplayer_isPlaying(L: Plua_State): integer; cdecl;
var parameters: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  lua_pop(L, lua_gettop(L));

  if xmplayer<>nil then
  begin
    result:=1;
    lua_pushboolean(L, xmplayer.isPlaying);
  end;
end;

function xmplayer_setVolume(L: Plua_State): integer; cdecl;
var parameters: integer;
  v: integer;
begin
  if xmplayer=nil then
    xmplayer:=TXMPlayer.create;

  result:=0;
  parameters:=lua_gettop(L);
  if (xmplayer<>nil) and (parameters=1) then
  begin
    v:=lua_tointeger(L,-1);
    xmplayer.setVolume(v);
  end;

  lua_pop(L, lua_gettop(L));
end;

procedure xmplayer_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setVolume', xmplayer_setVolume);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isPlaying', xmplayer_isPlaying);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'playXM', xmplayer_playXM);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'pause', xmplayer_pause);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'resume', xmplayer_resume);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'stop', xmplayer_stop);
end;

procedure initializeLuaXMPlayer;
begin
  Lua_RegisterObject('xmplayer',xmplayer);
  Lua_register(LuaVM, 'xmplayer_playXM', xmplayer_playXM);
  Lua_register(LuaVM, 'xmplayer_pause', xmplayer_pause);
  Lua_register(LuaVM, 'xmplayer_resume', xmplayer_resume);
  Lua_register(LuaVM, 'xmplayer_stop', xmplayer_stop);
  Lua_register(LuaVM, 'xmplayer_isPlaying', xmplayer_isPlaying);
end;

initialization
  luaclass_register(TXMPlayer, xmplayer_addMetaData);

end.

