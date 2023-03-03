unit LuaThread;

{
This unit contains the class used to control the threads spawned by lua
}

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows, comobj,
  {$endif}
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler, syncobjs,
  SyncObjs2;

procedure initializeLuaThread;

implementation

uses LuaClass, LuaObject;

resourcestring
  rsErrorInNativeThreadCalled = 'Error in thread called ';
  rsInNativeCode = ' in native code:';
  rsInvalidFirstParameterForCreateNativeThread = 'Invalid first parameter for createNativeThread';

type
  TCEThread=class (TThread)
  private
    fname: string;
    functionid: integer;
    L: PLua_State;
    newstate: boolean;
    newstate_script: string;
    fresult: string;
    procedure setname(newname: string);
  public
    syncfunction: integer;
    syncparam: integer;
    syncparamcount: integer;
    procedure sync; //called by lua_synchronize from inside the thread
    procedure execute; override;
    destructor destroy; override;
    constructor create(L: Plua_State; functionid: integer; suspended: boolean); overload;
    constructor create(script: string; suspended: boolean); overload;
  published
    property name: string read fname write setname;
    property Terminated;
    property Finished;
    property Result: string read fresult;
  end;


procedure TCEThread.sync;
var
  paramcount: integer;
  i: integer;
begin
  //call the lua function
  lua_rawgeti(L, LUA_REGISTRYINDEX, syncfunction);
  luaclass_newclass(L, self);

  if syncparam>0 then
  begin
    if syncparamcount=0 then
      syncparamcount:=1;

    for i:=0 to syncparamcount-1 do
      lua_pushvalue(L, syncparam+i);

    paramcount:=1+syncparamcount;
  end
  else
  begin
    lua_pushnil(L);
    paramcount:=2;
  end;

  lua_pcall(L, paramcount,1,0);
end;

procedure TCEThread.setname(newname: string);
begin
  fname:=newname;
  NameThreadForDebugging(newname,GetCurrentThreadId);
end;

procedure TCEThread.execute;
var errorstring: string;
  extraparamcount: integer;

  newstateScriptWrapper: TStringlist;
  r: integer;
begin
  //call the lua function
  {$ifdef windows}
  CoInitializeEx(nil,0);
  {$endif}

  try
    if newstate then
    begin
      newstateScriptWrapper:=Tstringlist.Create;
      newstateScriptWrapper.Insert(0,'return function(t)');
      newstateScriptWrapper.AddText(newstate_script);
      newstateScriptWrapper.add('end');

      r:=lua_dostring(L, pchar(newstateScriptWrapper.text));
      newstateScriptWrapper.free;

      if r<>0 then
      begin
        if lua_isstring(L, -1) then
          errorstring:=':'+Lua_ToString(L,-1)
        else
          errorstring:='';

        lua_getglobal(L, 'print');
        lua_pushstring(L, rsErrorInNativeThreadCalled+name+':'+errorstring);
        lua_pcall(L, 1,0,0);
        exit;
      end;

      //there is a function on the stack now

      extraparamcount:=0; //only lua state
    end
    else
    begin
      extraparamcount:=lua_gettop(L);

      lua_rawgeti(L, LUA_REGISTRYINDEX, functionid);

      lua_insert(L, 1);
    end;


    luaclass_newClass(L, self);
    lua_insert(L, 2);

    if lua_pcall(L, 1+extraparamcount,1,0)<>0 then
    begin
      if lua_isstring(L, -1) then
        errorstring:=':'+Lua_ToString(L,-1)
      else
        errorstring:='';

      lua_getglobal(L, 'print');
      lua_pushstring(L, rsErrorInNativeThreadCalled+name+':'+errorstring);
      lua_pcall(L, 1,0,0);
    end
    else
    begin
      fresult:=Lua_ToString(L,-1);
      lua_pop(L,1);
    end;
  except
    on e:Exception do
    begin
      lua_getglobal(L, 'print');
      lua_pushstring(L, rsErrorInNativeThreadCalled+name+rsInNativeCode+e.Message);
      lua_pcall(L, 1,0,0);
    end;
  end;

  OutputDebugString('Lua thread terminated');
end;

destructor TCEThread.destroy;
begin
  //dereference the function
  luaL_unref(L, LUA_REGISTRYINDEX, functionid);

  lua_pushnil(L);
  lua_setglobal(L, pchar('CELUATHREAD_'+IntToHex(ptruint(L),8)));


  if newstate then
    lua_close(L);


  inherited destroy;
end;

constructor TCEThread.create(L: PLua_state; functionid: integer; suspended: boolean);
begin
  self.l:=l;
  self.functionid:=functionid;
  name:='Unnamed';

  inherited create(suspended);
end;

constructor TCEThread.create(script: string; suspended: boolean);
begin
  //limited thread, it has to create the lua state itself, and also destroy it when done
  newstate:=true;
  l:=luaL_newstate;
  luaL_openlibs(L);
  luaHandler.InitLimitedLuastate(L);

  newstate_script:=script;
  name:='Unnamed newstate';

  inherited create(suspended);
end;



function createNativeThreadInternal(L: PLua_State; suspended: boolean): integer; cdecl;
var
  f: integer;
  routine: string;

  c: TCEThread;

  newL: Plua_State;
  s: string;

  paramcount: integer;
  i,v: integer;
begin
  result:=0;

  paramcount:=lua_gettop(L);

  if paramcount>=1 then
  begin
    if lua_isfunction(L,1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      //get a reference to this function

      lua_getglobal(L, pchar(routine));
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
      raise exception.create(rsInvalidFirstParameterForCreateNativeThread);

    newL:=lua_newthread(L);

    s:='CELUATHREAD_'+IntToHex(ptruint(newL),8);
    lua_setglobal(L, pchar(s));

    lua_sethook(newL, nil, 0, 0);   //no debugging on this thread for now

    //copy the extra parameters from the original call to the new one
    for i:=2 to paramcount do
    begin
      //get a ref to this parameter
      lua_pushvalue(L, i);
      v:=luaL_ref(L,  LUA_REGISTRYINDEX);
      lua_rawgeti(newL,LUA_REGISTRYINDEX,  v);

      lua_unref(L, v);
    end;

    //clear the stack  (just in case)
    lua_pop(L, lua_gettop(L));



    c:=TCEThread.create(newL, f, true);
    luaclass_newClass(L, c);
    result:=1;

    c.FreeOnTerminate:=true;

    if not suspended then
      c.Start;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

//Lua functions

function luaCreateCriticalSection(L: PLua_State): integer; cdecl;
var cs: TCriticalSection;
begin
  cs:=TCriticalSection.Create;
  luaclass_newclass(L, cs);
  result:=1;
end;

function luaCreateEvent(L: PLua_State): integer; cdecl;
var e: TEvent;
begin
  result:=0;
  if lua_gettop(L)=2 then
  begin
    e:=TEvent.Create(nil, lua_toboolean(L,1), lua_toboolean(L,2), '');
    luaclass_newclass(L, e);
    result:=1;
  end;
end;

function luaCreateSemaphore(L: PLua_State): integer; cdecl;
var s: TSemaphore;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    s:=TSemaphore.Create(lua_tointeger(L,1));
    luaclass_newclass(L, s);
    result:=1;
  end;
end;

function luaCreateMultiReadExclusiveWriteSynchronizer(L: PLua_State): integer; cdecl;
var mrew: TMultiReadExclusiveWriteSynchronizer;
begin
  mrew:=TMultiReadExclusiveWriteSynchronizer.Create;
  luaclass_newclass(L, mrew);
  result:=1;
end;

function lua_getCPUCount(L: PLua_State): integer; cdecl;
begin
  result:=1;
  lua_pushinteger(L, GetCPUCount);
end;

function createNativeThread(L: PLua_State): integer; cdecl;
begin
  result:=createNativeThreadInternal(L, false);
end;

function createNativeThreadSuspended(L: PLua_State): integer; cdecl;
begin
  result:=createNativeThreadInternal(L, true);
end;

function createNativeThreadNewState(L: PLua_State): integer; cdecl;
var
  script: string;
  t: TCEThread;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    script:=Lua_ToString(L,1);

    if script<>'' then
    begin
      t:=TCEThread.create(script, false); //not an autodestroy thread, the result has to be able to be obtained
      luaclass_newClass(L, t);
      result:=1;
    end;
  end;
end;

function thread_freeOnTerminate(L: PLua_State): integer; cdecl;
var
  c: TCEThread;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.FreeOnTerminate:=lua_toboolean(L, 1);
end;

function thread_synchronize(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;

  c: TCEThread;

  newL: Plua_State;
  paramstart, paramcount: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L, @paramstart, @paramcount);


  if paramcount>=1 then
  begin
    if lua_isfunction(L,paramstart) then
    begin
      lua_pushvalue(L, paramstart);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

    end
    else
    if lua_isstring(L,paramstart) then
    begin
      routine:=lua_tostring(L,paramstart);
      //get a reference to this function

      lua_getglobal(L, pchar(routine));
      f:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
      exit;


    c.syncfunction:=f;
    if paramcount>=2 then
    begin
      c.syncparam:=paramstart+1;
      c.syncparamcount:=paramcount-1;
    end
    else
      c.syncparam:=0;

    c.Synchronize(c, c.sync);

    luaL_unref(L, LUA_REGISTRYINDEX, f);

    result:=1;
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

function thread_terminate(L: PLua_State): integer; cdecl;
begin
  TCEThread(luaclass_getClassObject(L)).Terminate;
  result:=0;
end;

function thread_suspend(L: PLua_State): integer; cdecl;
begin
  TCEThread(luaclass_getClassObject(L)).Suspend;
  result:=0;
end;

function thread_resume(L: PLua_State): integer; cdecl;
begin
  TCEThread(luaclass_getClassObject(L)).Resume;
  result:=0;
end;

procedure thread_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'freeOnTerminate', thread_freeOnTerminate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'synchronize', thread_synchronize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'waitfor', thread_waitfor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'terminate', thread_terminate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'suspend', thread_suspend);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'resume', thread_resume);
end;

function criticalsection_enter(L: PLua_State): integer; cdecl;
begin
  TCriticalSection(luaclass_getClassObject(L)).Enter;
  result:=0;
end;

function criticalsection_leave(L: PLua_State): integer; cdecl;
begin
  TCriticalSection(luaclass_getClassObject(L)).Leave;
  result:=0;
end;

function criticalsection_tryEnter(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, TCriticalSection(luaclass_getClassObject(L)).TryEnter);
  result:=1;
end;

procedure criticalsection_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'enter', criticalsection_enter);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'leave', criticalsection_leave);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'acquire', criticalsection_enter);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'release', criticalsection_leave);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'tryEnter', criticalsection_tryEnter);
end;

function event_resetEvent(L: PLua_State): integer; cdecl;
begin
  TEvent(luaclass_getClassObject(L)).ResetEvent;
  result:=0;
end;

function event_setEvent(L: PLua_State): integer; cdecl;
begin
  TEvent(luaclass_getClassObject(L)).setEvent;
  result:=0;
end;


function event_waitfor(L: PLua_State): integer; cdecl;
begin
  if lua_gettop(L)=0 then exit(0);

  lua_pushinteger(L, integer(TEvent(luaclass_getClassObject(L)).WaitFor(lua_tointeger(L, 1))));
  result:=1;
end;

procedure event_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'resetEvent', event_resetEvent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setEvent', event_setEvent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'waitFor', event_waitFor);
end;


function semaphore_acquire(L: PLua_State): integer; cdecl;
begin
  TSemaphore(luaclass_getClassObject(L)).Acquire;
  result:=0;
end;

function semaphore_release(L: PLua_State): integer; cdecl;
begin
  TSemaphore(luaclass_getClassObject(L)).Release;
  result:=0;
end;

procedure semaphore_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'acquire', semaphore_acquire);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'release', semaphore_release);
end;

function mrew_beginWrite(L: PLua_State): integer; cdecl;
begin
  TMultiReadExclusiveWriteSynchronizer(luaclass_getClassObject(L)).Beginwrite;
  result:=0;
end;

function mrew_endWrite(L: PLua_State): integer; cdecl;
begin
  TMultiReadExclusiveWriteSynchronizer(luaclass_getClassObject(L)).Endwrite;
  result:=0;
end;

function mrew_beginRead(L: PLua_State): integer; cdecl;
begin
  TMultiReadExclusiveWriteSynchronizer(luaclass_getClassObject(L)).BeginRead;
  result:=0;
end;

function mrew_endRead(L: PLua_State): integer; cdecl;
begin
  TMultiReadExclusiveWriteSynchronizer(luaclass_getClassObject(L)).EndRead;
  result:=0;
end;

procedure mrew_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginWrite', mrew_beginWrite);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endWrite', mrew_endWrite);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginRead', mrew_beginRead);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endRead', mrew_endRead);
end;


procedure initializeLuaThread;
begin
  lua_register(LuaVM, 'createNativeThread', createNativeThread);
  lua_register(LuaVM, 'createNativeThreadNewState', createNativeThreadNewState);
  lua_register(LuaVM, 'createNativeThreadSuspended', createNativeThreadSuspended);
  lua_register(LuaVM, 'createThread', createNativeThread);
  lua_register(LuaVM, 'createThreadNewState', createNativeThreadNewState);
  lua_register(LuaVM, 'createThreadSuspended', createNativeThreadSuspended);
  lua_register(LuaVM, 'getCPUCount', lua_getCPUCount);
  lua_register(LuaVM, 'thread_freeOnTerminate', thread_freeOnTerminate);
  lua_register(LuaVM, 'thread_synchronize', thread_synchronize);
  lua_register(LuaVM, 'thread_waitfor', thread_waitfor);

  lua_register(LuaVM, 'createCriticalSection', luaCreateCriticalSection);
  lua_register(LuaVM, 'createEvent', luaCreateEvent);
  lua_register(LuaVM, 'createSemaphore', luaCreateSemaphore);
  lua_register(LuaVM, 'createMultiReadExclusiveWriteSynchronizer', luaCreateMultiReadExclusiveWriteSynchronizer );
end;

initialization
  luaclass_register(TThread, thread_addMetaData);
 // luaclass_register(TCEThread, thread_addMetaData);

  luaclass_register(TCriticalSection, criticalsection_addMetaData);
  luaclass_register(TEvent, event_addMetaData);
  luaclass_register(TSemaphore, semaphore_addMetaData);
  luaclass_register(TMultiReadExclusiveWriteSynchronizer, mrew_addMetaData);


end.


