unit LuaClass;

//Handles some of the common used class code


{$mode delphi}

interface

uses
  Classes, SysUtils, controls, lua, lauxlib, lualib, math;

type TAddMetaDataFunction=procedure(L: PLua_state; metatable: integer; userdata: integer );

function luaclass_createMetaTable(L: Plua_State): integer;

procedure luaclass_addClassFunctionToTable(L: PLua_State; metatable: integer; userdata: integer; functionname: string; f: lua_CFunction);
procedure luaclass_addPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; getfunction: lua_CFunction; setfunction: lua_CFunction);

procedure luaclass_addArrayPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; f: lua_CFunction);

procedure luaclass_canAutoDestroy(L: PLua_State; metatable: integer; state: boolean);

function luaclass_getClassObject(L: PLua_state): pointer; inline;

procedure luaclass_newClass(L: PLua_State; o: TObject); overload;
procedure luaclass_newClass(L: PLua_State; o: TObject; InitialAddMetaDataFunction: TAddMetaDataFunction); overload;
procedure luaclass_newClass(L: PLua_State; InitialAddMetaDataFunction: TAddMetaDataFunction); overload;

procedure luaclass_register(c: TClass; InitialAddMetaDataFunction: TAddMetaDataFunction);

implementation

uses LuaClassArray, LuaObject, LuaComponent;

var classlist: Tlist;

type
  TClasslistentry=record
    c: TClass;
    depth: integer;
    f: TAddMetaDataFunction;
  end;
  PClassListEntry=^TClassListEntry;


procedure luaclass_register(c: TClass; InitialAddMetaDataFunction: TAddMetaDataFunction);
//registers the classes that are accessible by lua. Used by findBestClassForObject
var cle: PClasslistentry;
    t: TClass;
begin
  if classlist=nil then
    classlist:=tlist.create;

  getmem(cle, sizeof(TClasslistentry));

  cle.c:=c;
  cle.depth:=0;
  cle.f:=InitialAddMetaDataFunction;
  t:=c;

  while t<>nil do
  begin
    inc(cle.depth);
    t:=t.ClassParent;
  end;

  classlist.Add(cle);
end;

function findBestClassForObject(O: TObject): TAddMetaDataFunction;
var lowest: TClass;
    i: integer;
    cle: PClassListEntry;

    best: TClasslistentry;
begin
  result:=nil;
  if o=nil then exit;

  best.depth:=0;
  best.c:=nil;
  best.f:=nil;

  if classlist<>nil then
  begin
    for i:=0 to classlist.Count-1 do
    begin
      cle:=classlist[i];
      if o.InheritsFrom(cle.c) and (cle.depth>best.depth) then
        best:=cle^;
    end;
  end;

  result:=best.f;
end;

procedure luaclass_newClass(L: PLua_State; InitialAddMetaDataFunction: TAddMetaDataFunction);
//converts the item at the top of the stack to a class object
var userdata, metatable: integer;
begin

  if Assigned(InitialAddMetaDataFunction) then
  begin
    userdata:=lua_gettop(L);

    metatable:=luaclass_createMetaTable(L);
    InitialAddMetaDataFunction(L, metatable, userdata);
    lua_setmetatable(L, userdata);
  end;
end;

procedure luaclass_newClass(L: PLua_State; o: TObject; InitialAddMetaDataFunction: TAddMetaDataFunction);
begin
  if (o<>nil) and (Assigned(InitialAddMetaDataFunction)) then
  begin
    lua_newuserdata(L, o);
    luaclass_newClass(L, InitialAddMetaDataFunction);
  end
  else
    lua_pushnil(L);
end;


procedure luaclass_newClass(L: PLua_State; o: TObject); overload;
var InitialAddMetaDataFunction: TAddMetaDataFunction;
begin
  if o<>nil then
  begin
    InitialAddMetaDataFunction:=findBestClassForObject(o);
    luaclass_newClass(L, o, InitialAddMetaDataFunction);
  end
  else
    lua_pushnil(L);
end;


function luaclass_getClassObject(L: PLua_state): pointer; inline;
//called by class functions. This is in case a 6.2 code executed the function manually
begin
  result:=pointer(lua_touserdata(L, ifthen(lua_type(L, lua_upvalueindex(1))=LUA_TUSERDATA, lua_upvalueindex(1), 1))^);
end;

procedure luaclass_addArrayPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; f: lua_CFunction);
var t,t2: integer;
begin
  lua_pushstring(L, propertyname);
  lua_newtable(L);

  t:=lua_gettop(L);

  luaclassarray_createMetaTable(L, userdata, f);
  lua_setmetatable(L, t);

  lua_settable(L, metatable);
end;

procedure luaclass_addPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; getfunction: lua_CFunction; setfunction: lua_CFunction);
var t: integer;
begin
  lua_pushstring(L, propertyname);
  lua_newtable(L);
  t:=lua_gettop(L);

  if assigned(getfunction) then
  begin
    lua_pushstring(L,'__get');
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, getfunction, 1);
    lua_settable(L, t);
  end;

  if assigned(setfunction) then
  begin
    lua_pushstring(L,'__set');
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, setfunction, 1);
    lua_settable(L, t);
  end;

  lua_settable(L, metatable);
end;

procedure luaclass_addClassFunctionToTable(L: PLua_State; metatable: integer; userdata: integer; functionname: string; f: lua_CFunction);
begin
  lua_pushstring(L, functionname);
  lua_pushvalue(L, userdata);
  lua_pushcclosure(L, f, 1);
  lua_settable(L, metatable);
end;

function luaclass_newindex(L: PLua_State): integer; cdecl; //set
//parameters: (self, key, newvalue)
begin
  result:=0;
  lua_getmetatable(L, 1); //get the metatable of self
  lua_pushvalue(L, 2); //push the key

  lua_gettable(L, -2); //metatable[key]
  if lua_istable(L ,-1) then
  begin
    lua_pushstring(L, '__set');
    lua_gettable(L, -2); //table['__set']

    if lua_isfunction(L, -1) then
    begin
      lua_pushvalue(L, 3); //push newvalue    (so stack now holds, function, newvalue)
      lua_call(L, 1, 0);
    end;
  end
  else
  begin
    if lua_isnil(L, -1) then
    begin
      lua_pop(L,1);

      //this entry was not in the list
      //Let's see if this is a published property
      lua_pushcfunction(L, lua_setProperty);
      lua_pushvalue(L, 1); //userdata
      lua_pushvalue(L, 2); //keyname
      lua_pushvalue(L, 3); //value
      lua_call(L,3,1);
    end;
  end;
end;

function luaclass_index(L: PLua_State): integer; cdecl; //get
//parameters: (self, key)
//called when a class object is indexed
//return the metatable element with this name

//wants to get the value of table[key] , but table isn'ty really a table
var i: integer;

    metatable: integer;

    s: string;
    o: TObject;
begin
  //return the metatable element


  i:=lua_gettop(L);

  result:=0;

  if i=2 then
  begin
    lua_getmetatable(L, 1); //get the metatable from the table and push i on the stack
    metatable:=lua_gettop(L);

    lua_pushvalue(L, 2); //push the key on the stack
    //lua_rawget(L, metatable);
    lua_gettable(L, metatable); //get metatable[key]

    if lua_istable(L ,-1) then
    begin
      //lua_getmetatable(L,-1);
      lua_pushstring(L, '__get');
      lua_gettable(L, -2);

      if lua_isfunction(L, -1) then
        lua_call(L, 0, 1);
    end
    else
    begin
      if lua_isnil(L, -1) then
      begin
        lua_pop(L,1);

        //this entry was not in the list
        //Let's see if this is a published property
        lua_pushcfunction(L, lua_getProperty);
        lua_pushvalue(L, 1); //userdata
        lua_pushvalue(L, 2); //keyname
        lua_call(L,2,1);

        if lua_isnil(L, -1) then
        begin
          //not a property
          o:=tobject(lua_touserdata(L,1)^);
          if o is TComponent then
          begin
            lua_pushcfunction(L, component_findComponentByName);
            lua_pushvalue(L, 1);
            lua_pushvalue(L, 2);
            lua_call(L, 2, 1);
          end;
        end;
      end;
    end;
    result:=1;
  end;

end;

function luaclass_garbagecollect(L: PLua_State): integer; cdecl; //gc
var autodestroy: boolean;
    o: tobject;
    mt: integer;
begin
  lua_getmetatable(L, 1);
  mt:=lua_gettop(L);
  lua_pushstring(L, '__autodestroy');
  lua_gettable(L, mt);
  autodestroy:=lua_toboolean(L, -1);
  if autodestroy then
  begin
    //kill it

    lua_pushstring(L, 'destroy');
    lua_gettable(L, mt);
    if lua_isfunction(L, -1) then
      lua_call(L, 0,0);

  end;

  result:=0;
end;



procedure luaclass_canAutoDestroy(L: PLua_State; metatable: integer; state: boolean);
begin
  lua_pushstring(L, '__autodestroy');
  lua_pushboolean(L, state);
  lua_settable(L, metatable);
end;

function luaclass_createMetaTable(L: Plua_State): integer;
//creates a table to be used as a metatable
//returns the stack index of the table
begin
  lua_newtable(L);
  result:=lua_gettop(L);

  luaclass_canAutoDestroy(L, result, false); //default do not destroy when garbage collected. Let the user do it

  //set the index method
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, luaclass_index);
  lua_settable(L, result);

  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, luaclass_newindex);
  lua_settable(L, result);

  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, luaclass_garbagecollect);
  lua_settable(L, result);
end;




end.

