unit LuaObject;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, math, typinfo, Controls,
  ComCtrls, StdCtrls, Forms;

procedure InitializeLuaObject;
procedure object_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

function lua_getProperty(L: PLua_state): integer; cdecl;
function lua_setProperty(L: PLua_state): integer; cdecl;


implementation

uses LuaClass, LuaHandler, pluginexports, LuaCaller, symbolhandler;

function object_destroy(L: PLua_State): integer; cdecl;
var c: TObject;
  metatable: integer;
  i: integer;
begin
  i:=ifthen(lua_type(L, lua_upvalueindex(1))=LUA_TUSERDATA, lua_upvalueindex(1), 1);
  c:=pointer(lua_touserdata(L, i)^);
  lua_getmetatable(L, i);
  metatable:=lua_gettop(L);

  try
    c.free;
  except
  end;

  if lua_type(L, metatable)=LUA_TTABLE then
  begin
    lua_pushstring(L, '__autodestroy');
    lua_pushboolean(L, false); //make it so it doesn't need to be destroyed (again)
    lua_settable(L, metatable);
  end;
end;

function object_getClassName(L: PLua_state): integer; cdecl;
var c: TObject;
begin
  c:=luaclass_getClassObject(L);
  lua_pushstring(L, c.ClassName);
  result:=1;
end;

procedure object_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //no parent class metadata to add
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getClassName', object_getClassName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'destroy', object_destroy);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ClassName', object_getClassName, nil);
end;

function getPropertyList(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: tobject;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_toceuserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, ce_getPropertylist(c));
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function lua_getProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c,c2: tobject;
  t: ptruint;
  p: string;


  svalue: string;

  size: integer;

  pinfo: PPropInfo;
  m: tmethod;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    if lua_isuserdata(L,1) then
      c:=lua_toceuserdata(L, 1)
    else
    if lua_isnumber(L,1) then
      c:=pointer(lua_tointeger(L,1))
    else
    begin
      p:=Lua_ToString(L,1);
      if p<>'' then
        c:=pointer(StrToInt64(p));
    end;

    p:=Lua_ToString(L, 2);

    lua_pop(L, lua_gettop(l));

    pinfo:=GetPropInfo(c, p);
    if pinfo<>nil then
    begin
      result:=1;
      { possible types:
      tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,
                         tkSet,tkMethod,tkSString,tkLString,tkAString,
                         tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                         tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
                         tkDynArray,tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
                         tkHelper
      }

      case pinfo.PropType.Kind of
        tkInteger,tkInt64,tkQWord: lua_pushinteger(L, GetPropValue(c, p,false));
        tkBool: lua_pushboolean(L, GetPropValue(c, p, false));
        tkFloat: lua_pushnumber(L, GetPropValue(c, p, false));
        tkClass, tkObject:
        begin
          t:=GetPropValue(c,p,false);
          c2:=tobject(t);
          luaclass_newClass(L, c2);
        end;
        tkMethod: LuaCaller_pushMethodProperty(L, GetMethodProp(c,p), pinfo.PropType.Name);
        tkSet: lua_pushstring(L, GetSetProp(c, pinfo, true));
        else lua_pushstring(L, GetPropValue(c, p,true));
      end;
    end
    else
    begin
      lua_pushnil(L);
      result:=1;
    end;
  end;
end;

function lua_setProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c,c2: tobject;
  p,v: string;
  pinfo: PPropInfo;
  f: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    if lua_isuserdata(L,1) then
      c:=lua_toceuserdata(L, 1)
    else
    if lua_isnumber(L,1) then
      c:=pointer(lua_tointeger(L,1))
    else
    begin
      p:=Lua_ToString(L,1);
      if p<>'' then
        c:=pointer(StrToInt64(p));
    end;

    p:=Lua_ToString(L, 2);
    v:=Lua_ToString(L, 3);

    try
      pinfo:=GetPropInfo(c, p);
      if pinfo<>nil then
      begin
        //it's a published property
        case pinfo.PropType.Kind of
          tkInteger,tkInt64,tkQWord:
          begin
            if lua_type(L,3)=LUA_TSTRING then
            begin
              //expected an integer, but got a string
              SetPropValue(c, p, symhandler.getAddressFromName(Lua_ToString(L, 3)));
            end
            else
              SetPropValue(c, p, lua_tointeger(L, 3));
          end;

          tkBool: SetPropValue(c, p, lua_toboolean(L, 3));
          tkFloat: SetPropValue(c, p, lua_tonumber(L, 3));
          tkClass, tkObject:
          begin
            c2:=lua_ToCEUserData(L, 3);
            SetPropValue(c, p, ptruint(c2));
          end;
          tkset: SetSetProp(c, pinfo, v);
          tkMethod: luacaller_setMethodProperty(L, c, p, pinfo.PropType.Name, 3);

          else SetPropValue(c, p, v)
        end;
      end;

    except
    end;
  end;

  lua_pop(L, lua_gettop(l));
end;


//6.2 only
function getMethodProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: tobject;
  p: string;
  pi: ppropinfo;
  m: TMethod;

  c2: tobject;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    if lua_isuserdata(L,1) then
      c:=lua_toceuserdata(L, 1)
    else
    if lua_isnumber(L,1) then
      c:=pointer(lua_tointeger(L,1))
    else
    begin
      p:=Lua_ToString(L,1);
      if p<>'' then
        c:=pointer(StrToInt64(p));
    end;

    p:=Lua_ToString(L,2);

    lua_pop(L, lua_gettop(L));

    m:=GetMethodProp(c,p);

    pi:=GetPropInfo(c,p);

    if (pi=nil) or (pi.proptype=nil) or (pi.PropType.Kind<>tkMethod) then
    begin
      raise exception.create('This is an invalid class or method property');
    end;


    if m.data<>nil then
    begin
      luaCaller_pushMethodProperty(L, m, pi.PropType.Name);
      result:=1;
    end
    else
    begin
      lua_pushnil(L);
      result:=1;
    end;
  end
  else
    lua_pop(L, lua_gettop(L));




end;

//6.2- only
function setMethodProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c: tobject;
  p: string;

  pi: ppropinfo;

  lc: TLuaCaller;
  m: TMethod;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    if lua_isuserdata(L,1) then
      c:=lua_toceuserdata(L, 1)
    else
    if lua_isnumber(L,1) then
      c:=pointer(lua_tointeger(L,1))
    else
    begin
      p:=Lua_ToString(L,1);
      if p<>'' then
        c:=pointer(StrToInt64(p));
    end;

    p:=Lua_ToString(L,2);

    lc:=TLuaCaller.create;

    if lua_isfunction(L, 3) then
    begin
      lua_pushvalue(L, 3);
      lc.luaroutineindex:=luaL_ref(L,LUA_REGISTRYINDEX)
    end
    else
    if lua_isnil(L,3) then
    begin
      //special case. nil the event
      lua_pop(L, lua_gettop(L));
      m.code:=nil;
      m.data:=nil;
      luacaller.setMethodProperty(c,p,m);
      exit;
    end
    else
      lc.luaroutine:=lua_tostring(L,3);

    lua_pop(L, lua_gettop(L));

    //look up the info of this property
    pi:=GetPropInfo(c,p);
    if (pi<>nil) and (pi.proptype<>nil) and (pi.PropType.Kind=tkMethod) then
    begin
      //it's a valid method property
      if pi.PropType.Name ='TNotifyEvent' then
        m:=tmethod(TNotifyEvent(lc.NotifyEvent))
      else
      if pi.PropType.Name ='TSelectionChangeEvent' then
        m:=tmethod(TSelectionChangeEvent(lc.SelectionChangeEvent))
      else
      if pi.PropType.Name ='TCloseEvent' then
        m:=tmethod(TCloseEvent(lc.CloseEvent))
      else
      if pi.PropType.Name ='TMouseEvent' then
        m:=tmethod(TMouseEvent(lc.MouseEvent()))
      else
      if pi.PropType.Name ='TMouseMoveEvent' then
        m:=tmethod(TMouseMoveEvent(lc.MouseMoveEvent))
      else
      if pi.PropType.Name ='TKeyPressEvent' then
        m:=tmethod(TKeyPressEvent(lc.KeyPressEvent))
      else
      if pi.PropType.Name ='TLVCheckedItemEvent' then
        m:=tmethod(TLVCheckedItemEvent(lc.LVCheckedItemEvent))
      else
      begin
        lc.free;
        raise exception.create('This type of method:'+pi.PropType.Name+' is not yet supported');
      end;

      luacaller.setMethodProperty(c,p,m);

    end
    else
    begin
      lc.free;
      raise exception.create('This is an invalid class or method property');
    end;


  end
  else
    lua_pop(L, lua_gettop(L));
end;


procedure InitializeLuaObject;
begin
  lua_register(LuaVM, 'getPropertyList', getPropertyList);
  lua_register(LuaVM, 'setProperty', lua_setProperty);
  lua_register(LuaVM, 'getProperty', lua_getProperty);
  lua_register(LuaVM, 'setMethodProperty', setMethodProperty);
  lua_register(LuaVM, 'getMethodProperty', getMethodProperty);

  lua_register(LuaVM, 'object_getClassName', object_getClassName);
  lua_register(LuaVM, 'object_destroy', object_destroy);

  luaclass_register(TObject, object_addMetaData); //so it will ALWAYS find at least one thing
end;

end.

