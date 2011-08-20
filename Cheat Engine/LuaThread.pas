unit LuaThread;

{
This unit contains the class used to control the threads spawned by lua
}

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler, dialogs;

procedure initializeLuaThread;

implementation

type TCEThread=class (TThread)
  private
    functionid: integer;
    L: PLua_State;
  public
    syncfunction: integer;
    procedure sync; //called by lua_synchronize from inside the thread
    procedure execute; override;
    destructor destroy; override;
    constructor create(L: Plua_State; functionid: integer; suspended: boolean);
end;

procedure TCEThread.sync;
begin
  //call the lua function
  lua_rawgeti(L, LUA_REGISTRYINDEX, syncfunction);
  lua_pcall(L, 0,0,0);
end;

procedure TCEThread.execute;
begin
  //call the lua function
  lua_rawgeti(L, LUA_REGISTRYINDEX, functionid);
  lua_pushlightuserdata(L, self);
  lua_pcall(L, 1,0,0);
end;

destructor TCEThread.destroy;
begin
  //dereference the function
  luaL_unref(L, LUA_REGISTRYINDEX, functionid);

  lua_pushnil(L);
  lua_setglobal(L, pchar('CELUATHREAD_'+IntToHex(ptruint(self),8)));

  inherited destroy;
end;

constructor TCEThread.create(L: PLua_state; functionid: integer; suspended: boolean);
begin
  self.l:=l;
  self.functionid:=functionid;

  inherited create(suspended);
end;


//Lua functions
function createNativeThread(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  f: integer;
  routine: string;

  c: TCEThread;

  newL: Plua_State;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      //get a reference to this function

      lua_getfield(L, LUA_GLOBALSINDEX, pchar(routine));
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
    end;

    newL:=lua_newthread(L);
    lua_setglobal(L, pchar('CELUATHREAD_'+IntToHex(ptruint(newL),8)));


    //clear the stack  (just in case)
    lua_pop(L, lua_gettop(L));

    result:=1;

    c:=TCEThread.create(newL, f, true);
    lua_pushlightuserdata(L, c);

    c.FreeOnTerminate:=true;
    c.Start;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function thread_freeOnTerminate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TCEThread;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -parameters);
    state:=lua_toboolean(L, -parameters+1);
    c.FreeOnTerminate:=state;
  end;

  lua_pop(L, lua_gettop(L));
end;

function thread_synchronize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  f: integer;
  routine: string;

  c: TCEThread;

  newL: Plua_State;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L, -2);

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      //get a reference to this function

      lua_getfield(L, LUA_GLOBALSINDEX, pchar(routine));
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
    end;


    c.syncfunction:=f;
    lua_pop(L, lua_gettop(L));

    c.Synchronize(c, c.sync);

    luaL_unref(L, LUA_REGISTRYINDEX, f);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function thread_waitfor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TCEThread;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L, -parameters);
    c.WaitFor;
  end;

  lua_pop(L, lua_gettop(L));
end;

procedure initializeLuaThread;
begin
  lua_register(LuaVM, 'createNativeThread', createNativeThread);
  lua_register(LuaVM, 'thread_freeOnTerminate', thread_freeOnTerminate);
  lua_register(LuaVM, 'thread_synchronize', thread_synchronize);
  lua_register(LuaVM, 'thread_waitfor', thread_waitfor);
end;


end.

