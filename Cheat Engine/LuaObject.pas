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

resourcestring
  rsThisIsAnInvalidClassOrMethodProperty = 'This is an invalid class or method property';
  rsThisTypeOfMethod = 'This type of method:';
  rsIsNotYetSupported = ' is not yet supported';

function object_destroy(L: PLua_State): integer; cdecl;
var c: TObject;
  metatable: integer;
  i,count: integer;
  proplist: PPropList;
  m: TMethod;
  ma: array of TMethod;
begin
  result:=0;

  i:=ifthen(lua_type(L, lua_upvalueindex(1))=LUA_TUSERDATA, lua_upvalueindex(1), 1);
  c:=lua_toceuserdata(L, i);
  lua_getmetatable(L, i);
  metatable:=lua_gettop(L);

  try
    //now cleanup the callers

    if (c is TCustomForm) and assigned(TCustomForm(c).OnDestroy) then
    begin
      try
        TCustomForm(c).OnDestroy(c);
      except
        //don't care
      end;
      TCustomForm(c):=nil;
    end;

    count:=GetPropList(c, proplist);
    for i:=0 to count-1 do
    begin
      if proplist[i]^.PropType.Kind=tkMethod then
      begin
        m:=GetMethodProp(c, proplist[i]);

        if (proplist[i]^.Name='OnDestroy') then
        begin
          if (m.Code<>nil) and (m.data<>nil) then
            TNotifyEvent(m)(c);
        end;

        CleanupLuaCall(m);
        m.Code:=nil;
        m.data:=nil;
        SetMethodProp(c, proplist[i], m);
      end;
    end;

    c.free;
  except
  end;

  if lua_type(L, metatable)=LUA_TTABLE then
  begin
    lua_pushstring(L, '__autodestroy');
    lua_pushboolean(L, false); //make it so it doesn't need to be destroyed (again)
    lua_settable(L, metatable);

    lua_pushstring(L, '__destroyed'); //it has been destroyed, but the luaside needs to garbage collect it still
    lua_pushboolean(L, true);
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

function object_fieldAddress(L: PLua_state): integer; cdecl;
var c: TObject;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, ptruint(c.FieldAddress(lua_tostring(L,1))));
  result:=1;
end;

function object_methodAddress(L: PLua_state): integer; cdecl;
var c: TObject;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, ptruint(c.MethodAddress(lua_tostring(L,1))));
  result:=1;
end;

function object_methodName(L: PLua_state): integer; cdecl;
var c: TObject;
begin
  c:=luaclass_getClassObject(L);
  lua_pushstring(L, c.MethodName(pointer(lua_tointeger(L,1))));
  result:=1;
end;

procedure object_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //no parent class metadata to add
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getClassName', object_getClassName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fieldAddress', object_fieldAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'methodAddress', object_methodAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'methodName', object_methodName);
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

    luaclass_newClass(L, ce_getPropertylist(c));

    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function lua_getProperty(L: PLua_state): integer; cdecl;
var parameters: integer;
  c,c2: tobject;
  t: ptruint;
  p,v: string;


  svalue: string;

  size: integer;

  pinfo: PPropInfo;
  m: tmethod;
  kind: TTypeKind;
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
        c:=pointer(StrToInt64(p))
      else
        exit;
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
      kind:=pinfo^.PropType.Kind;
      case kind of
        tkInteger,tkInt64,tkQWord: lua_pushinteger(L, GetPropValue(c, p,false));
        tkBool: lua_pushboolean(L, GetPropValue(c, p, false));
        tkFloat: lua_pushnumber(L, GetPropValue(c, p, false));
        tkClass, tkObject: luaclass_newClass(L, GetObjectProp(c, p));
        tkMethod: LuaCaller_pushMethodProperty(L, GetMethodProp(c,p), pinfo.PropType.Name);
        tkSet: lua_pushstring(L, GetSetProp(c, pinfo, true));
        else
        begin
          v:=GetPropValue(c, p,true);
          lua_pushstring(L, v);
        end;
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

  metatable: integer;
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
            //if lua_type(L,3)=LUA_TSTRING then
            //begin
              //expected an integer, but got a string
              //SetPropValue(c, p, symhandler.getAddressFromName(Lua_ToString(L, 3)));
           // end
            //else
              try
                SetPropValue(c, p, v);
              except
                SetPropValue(c,p, lua_toInteger(L, 3));
              end;
          end;

          tkBool: SetPropValue(c, p, lua_toboolean(L, 3));
          tkFloat: SetPropValue(c, p, lua_tonumber(L, 3));
          tkClass, tkObject:
          begin
            c2:=lua_ToCEUserData(L, 3);
            SetObjectProp(c, p, c2);
          end;
          tkset: SetSetProp(c, pinfo, v);
          tkMethod: luacaller_setMethodProperty(L, c, p, pinfo.PropType.Name, 3);

          tkEnumeration:
          begin
            if lua_isnumber(L,3) then
            begin
              v:=GetEnumName(pinfo.PropType, lua_tointeger(L, 3));
              SetEnumProp(c,p, v);
            end
            else
              SetPropValue(c, p, v)
          end

          else SetPropValue(c, p, v)
        end;
      end
      else
      begin
        //not a property
        lua_getmetatable(L, 1);
        metatable:=lua_gettop(L);

        lua_pushvalue(L, 2);
        lua_pushvalue(L, 3);
        lua_settable(L, metatable);


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
        c:=pointer(StrToInt64(p))
      else
        exit;
    end;

    p:=Lua_ToString(L,2);

    lua_pop(L, lua_gettop(L));

    m:=GetMethodProp(c,p);

    pi:=GetPropInfo(c,p);

    if (pi=nil) or (pi.proptype=nil) or (pi.PropType.Kind<>tkMethod) then
    begin
      raise exception.create(rsThisIsAnInvalidClassOrMethodProperty);
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
        c:=pointer(StrToInt64(p))
      else
        exit;
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
      if pi.PropType.Name ='TMouseWheelUpDownEvent' then
        m:=tmethod(TMouseWheelUpDownEvent(lc.MouseWheelUpDownEvent))
      else
      if pi.PropType.Name ='TKeyPressEvent' then
        m:=tmethod(TKeyPressEvent(lc.KeyPressEvent))
      else
      if pi.PropType.Name ='TLVCheckedItemEvent' then
        m:=tmethod(TLVCheckedItemEvent(lc.LVCheckedItemEvent))
      else
      begin
        lc.free;
        raise exception.create(rsThisTypeOfMethod+pi.PropType.Name+rsIsNotYetSupported);
      end;

      luacaller.setMethodProperty(c,p,m);

    end
    else
    begin
      lc.free;
      raise exception.create(rsThisIsAnInvalidClassOrMethodProperty);
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
end;

initialization
  luaclass_register(TObject, object_addMetaData); //so it will ALWAYS find at least one thing

end.

