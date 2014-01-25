unit LuaThread;

{
This unit contains the class used to control the threads spawned by lua
}

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaThread;

implementation

uses luaclass, LuaObject;

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
  luaclass_newclass(L, self);
  lua_pcall(L, 1,0,0);
end;

procedure TCEThread.execute;
var errorstring: string;
begin
  //call the lua function
  try
    lua_rawgeti(L, LUA_REGISTRYINDEX, functionid);
    lua_pushlightuserdata(L, self);
    if lua_pcall(L, 1,0,0)<>0 then
    begin
      if lua_isstring(L, -1) then
        errorstring:=':'+Lua_ToString(L,-1)
      else
        errorstring:='';

      lua_getfield(L, LUA_GLOBALSINDEX, 'print');
      lua_pushstring(L, 'Error in native thread'+errorstring);
      lua_pcall(L, 1,0,0);

    end;
  except
    on e:Exception do
    begin
      lua_getfield(L, LUA_GLOBALSINDEX, 'print');
      lua_pushstring(L, 'Error in native thread in native code:'+e.Message);
      lua_pcall(L, 1,0,0);
    end;
  end;
end;

destructor TCEThread.destroy;
begin
  //dereference the function
  luaL_unref(L, LUA_REGISTRYINDEX, functionid);

  lua_pushnil(L);
  lua_setglobal(L, pchar('CELUATHREAD_'+IntToHex(ptruint(L),8)));

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
  f: integer;
  routine: string;

  c: TCEThread;

  newL: Plua_State;
  s: string;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L,1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      //get a reference to this function

      lua_getfield(L, LUA_GLOBALSINDEX, pchar(routine));
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
    end;

    newL:=lua_newthread(L);
    s:='CELUATHREAD_'+IntToHex(ptruint(newL),8);
    lua_setglobal(L, pchar(s));


    //clear the stack  (just in case)
    lua_pop(L, lua_gettop(L));



    c:=TCEThread.create(newL, f, true);
    luaclass_newClass(L, c);
    result:=1;

    c.FreeOnTerminate:=true;
    c.Start;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function thread_freeOnTerminate(L: PLua_State): integer; cdecl;
var
  c: TCEThread;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.FreeOnTerminate:=lua_toboolean(L, -1);
end;

function thread_synchronize(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;

  c: TCEThread;

  newL: Plua_State;
begin
  result:=0;
  c:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
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


    c.syncfunction:=f;
    lua_pop(L, lua_gettop(L));

    c.Synchronize(c, c.sync);

    luaL_unref(L, LUA_REGISTRYINDEX, f);
  end;
end;

function thread_waitfor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TCEThread;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  c.WaitFor;
end;

procedure thread_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'freeOnTerminate', thread_freeOnTerminate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'synchronize', thread_synchronize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'waitfor', thread_waitfor);
end;

procedure initializeLuaThread;
begin
  lua_register(LuaVM, 'createNativeThread', createNativeThread);
  lua_register(LuaVM, 'thread_freeOnTerminate', thread_freeOnTerminate);
  lua_register(LuaVM, 'thread_synchronize', thread_synchronize);
  lua_register(LuaVM, 'thread_waitfor', thread_waitfor);
end;

initialization
  luaclass_register(TThread, thread_addMetaData);


end.


