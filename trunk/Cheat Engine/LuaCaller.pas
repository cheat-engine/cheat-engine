unit LuaCaller;
{
The luaCaller is a class which contains often defined Events and provides an
interface for gui objects to directly call the lua functions with proper parameters
and results
}

{$mode delphi}

interface

uses
  Classes, Controls, SysUtils, ceguicomponents, forms, lua, lualib, lauxlib,
  comctrls, StdCtrls, CEFuncProc, typinfo;

type
  TLuaCaller=class
    private
      function canRun: boolean;

    public
      luaroutine: string;
      luaroutineindex: integer;
      owner: TPersistent;
      procedure NotifyEvent(sender: TObject);
      procedure SelectionChangeEvent(Sender: TObject; User: boolean);
      procedure MouseEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure KeyPressEvent(Sender: TObject; var Key: char);
      procedure LVCheckedItemEvent(Sender: TObject; Item: TListItem); //personal request to have this one added
      procedure CloseEvent(Sender: TObject; var CloseAction: TCloseAction);
      function ActivateEvent(sender: TObject; before, currentstate: boolean): boolean;
      procedure DisassemblerSelectionChangeEvent(sender: TObject; address, address2: ptruint);
      procedure ByteSelectEvent(sender: TObject; address: ptruint; address2: ptruint);
      procedure AddressChangeEvent(sender: TObject; address: ptruint);
      function AutoGuessEvent(address: ptruint; originalVariableType: TVariableType): TVariableType;
      procedure D3DClickEvent(renderobject: TObject; x,y: integer);
      function D3DKeyEvent(VirtualKey: dword; char: pchar): boolean;

      procedure pushFunction;


      constructor create;
      destructor destroy; override;
  end;

procedure CleanupLuaCall(event: TMethod);   //cleans up a luacaller class if it was assigned if it was set

procedure setMethodProperty(O: TObject; propertyname: string; method: TMethod);

function LuaCaller_NotifyEvent(L: PLua_state): integer; cdecl;
function LuaCaller_SelectionChangeEvent(L: PLua_state): integer; cdecl;
function LuaCaller_CloseEvent(L: PLua_state): integer; cdecl;
function LuaCaller_MouseEvent(L: PLua_state): integer; cdecl;
function LuaCaller_MouseMoveEvent(L: PLua_state): integer; cdecl;
function LuaCaller_KeyPressEvent(L: PLua_state): integer; cdecl;
function LuaCaller_LVCheckedItemEvent(L: PLua_state): integer; cdecl;

procedure LuaCaller_pushMethodProperty(L: PLua_state; m: TMethod; typename: string);

implementation

uses luahandler, MainUnit;

procedure luaCaller_pushMethodProperty(L: PLua_state; m: TMethod; typename: string);
begin
  if m.data=nil then
  begin
    lua_pushnil(L);
    exit;
  end;

  if tobject(m.Data)is TLuaCaller then
    TLuaCaller(m.data).pushFunction
  else
  begin
    //not a lua function

    //this can (and often is) a class specific thing

    lua_pushlightuserdata(L, m.code);
    lua_pushlightuserdata(L, m.data);

    if typename ='TNotifyEvent' then
      lua_pushcclosure(L, LuaCaller_NotifyEvent,2)
    else
    if typename ='TSelectionChangeEvent' then
      lua_pushcclosure(L, LuaCaller_SelectionChangeEvent,2)
    else
    if typename ='TCloseEvent' then
      lua_pushcclosure(L, LuaCaller_CloseEvent,2)
    else
    if typename ='TMouseEvent' then
      lua_pushcclosure(L, LuaCaller_MouseEvent,2)
    else
    if typename ='TMouseMoveEvent' then
      lua_pushcclosure(L, LuaCaller_MouseMoveEvent,2)
    else
    if typename ='TKeyPressEvent' then
      lua_pushcclosure(L, LuaCaller_KeyPressEvent,2)
    else
    if typename ='TLVCheckedItemEvent' then
      lua_pushcclosure(L, LuaCaller_LVCheckedItemEvent,2)
    else
      raise exception.create('This type of method:'+typename+' is not yet supported');
  end;
end;

procedure CleanupLuaCall(event: TMethod);
begin
  if (event.code<>nil) and (event.data<>nil) and (TObject(event.data) is TLuaCaller) then
    TLuaCaller(event.data).free;
end;

procedure setMethodProperty(O: TObject; propertyname: string; method: TMethod);
var orig: TMethod;
begin
  orig:=GetMethodProp(o, propertyname);
  CleanupLuaCall(orig);
  SetMethodProp(O, propertyname, method);
end;

constructor TLuaCaller.create;
begin
  luaroutineindex:=-1;
end;

destructor TLuaCaller.destroy;
begin
  if luaroutineindex<>-1 then //deref
    luaL_unref(luavm, LUA_REGISTRYINDEX, luaroutineindex);
end;

function TLuaCaller.canRun: boolean;
var baseOwner: TComponent;
begin
  baseOwner:=Tcomponent(owner);
  if baseOwner<>nil then
  begin
    while (not (baseOwner is TCustomForm)) and (baseowner.Owner<>nil) do //as long as the current base is not a form and it still has a owner
      baseOwner:=baseowner.owner;
  end;

  result:=(baseowner=nil) or (not ((baseOwner is TCEform) and (TCEForm(baseowner).designsurface<>nil) and (TCEForm(baseowner).designsurface.active)));
end;

procedure TLuaCaller.pushFunction;
begin
  if luaroutineindex=-1 then //get the index of the given routine
    lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(luaroutine))
  else
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, luaroutineindex)
end;

procedure TLuaCaller.SelectionChangeEvent(Sender: TObject; User: boolean);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);
      lua_pushboolean(Luavm, User);

      lua_pcall(Luavm, 2,0,0); //procedure(sender)
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.NotifyEvent(sender: TObject);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);

      lua_pcall(Luavm, 1,0,0); //procedure(sender)
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.CloseEvent(Sender: TObject; var CloseAction: TCloseAction);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);


      if lua_pcall(Luavm, 1,1,0)=0 then //procedure(sender)  lua_pcall returns 0 if success
      begin
        if lua_gettop(Luavm)>0 then
          CloseAction:=TCloseAction(lua_tointeger(LuaVM,-1));
      end
      else
        closeAction:=caHide; //not implemented by the user

      if mainform.mustclose then
        closeaction:=cahide;

    end
    else closeaction:=caHide;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

function TLuaCaller.ActivateEvent(sender: tobject; before, currentstate: boolean): boolean;
var oldstack: integer;
begin
  result:=true;
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);
      lua_pushboolean(luavm, before);
      lua_pushboolean(luavm, currentstate);


      lua_pcall(Luavm, 3,1,0); //function(sender, before, currentstate):boolean

      if lua_gettop(Luavm)>0 then
        result:=lua_toboolean(LuaVM,-1);

    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.DisassemblerSelectionChangeEvent(sender: tobject; address, address2: ptruint);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);
      lua_pushinteger(luavm, address);
      lua_pushinteger(luavm, address2);


      lua_pcall(Luavm, 3,0,0); //procedure(sender, address, address2)
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.ByteSelectEvent(sender: TObject; address: ptruint; address2: ptruint);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);
      lua_pushinteger(luavm, address);
      lua_pushinteger(luavm, address2);

      lua_pcall(Luavm, 3,0,0); //procedure(sender, address, address2)
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

function TLuaCaller.D3DKeyEvent(VirtualKey: dword; char: pchar): boolean;
var oldstack: integer;
begin
  result:=true;
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushinteger(luavm, VirtualKey);
      lua_pushstring(luavm, char);
      if lua_pcall(Luavm, 2,1,0)=0 then
        result:=lua_toboolean(luavm,-1);
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.D3DClickEvent(renderobject: TObject; x,y: integer);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(luavm, renderobject);
      lua_pushinteger(luavm, x);
      lua_pushinteger(luavm, y);
      lua_pcall(Luavm, 3,0,0)
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.AddressChangeEvent(sender: TObject; address: ptruint);
var oldstack: integer;
begin
  Luacs.Enter;
  try
    oldstack:=lua_gettop(Luavm);

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);
      lua_pushinteger(luavm, address);

      lua_pcall(Luavm, 2,0,0); //procedure(sender, address)
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;



function TLuaCaller.AutoGuessEvent(address: ptruint; originalVariableType: TVariableType): TVariableType;
var oldstack: integer;
begin
  Luacs.enter;
  try
    oldstack:=lua_gettop(Luavm);

    PushFunction;
    lua_pushinteger(luavm, address);
    lua_pushinteger(luavm, integer(originalVariableType));
    if lua_pcall(LuaVM, 2, 1, 0)=0 then         // lua_pcall returns 0 if success
      result:=TVariableType(lua_tointeger(LuaVM,-1))
    else
      result:=originalVariableType;




  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var oldstack: integer;
begin
  Luacs.enter;
  try
    oldstack:=lua_gettop(Luavm);
    pushFunction;
    lua_pushlightuserdata(luavm, sender);
    lua_pushinteger(luavm, x);
    lua_pushinteger(luavm, y);

    lua_pcall(LuaVM, 3, 0, 0);
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.MouseEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var oldstack: integer;
begin
  Luacs.enter;
  try
    oldstack:=lua_gettop(Luavm);
    pushFunction;
    lua_pushlightuserdata(luavm, sender);
    lua_pushinteger(luavm, integer(Button));
    lua_pushinteger(luavm, x);
    lua_pushinteger(luavm, y);

    lua_pcall(LuaVM, 4, 0, 0);
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.KeyPressEvent(Sender: TObject; var Key: char);
var oldstack: integer;
  s: string;
begin
  Luacs.enter;
  try
    oldstack:=lua_gettop(Luavm);
    pushFunction;
    lua_pushlightuserdata(luavm, sender);
    lua_pushstring(luavm, key);
    if lua_pcall(LuaVM, 2, 1, 0)=0 then  //lua_pcall returns 0 if success
    begin
      if lua_isstring(LuaVM, -1) then
      begin
        s:=lua_tostring(LuaVM,-1);
        if length(s)>0 then
          key:=s[1]
        else
          key:=#0; //invalid string
      end
      else
      if lua_isnumber(LuaVM, -1) then
        key:=chr(lua_tointeger(LuaVM, -1))
      else
        key:=#0; //invalid type returned
    end;
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end;
end;

procedure TLuaCaller.LVCheckedItemEvent(Sender: TObject; Item: TListItem);
var oldstack: integer;
begin
  Luacs.enter;
  try
    oldstack:=lua_gettop(Luavm);
    pushFunction;
    lua_pushlightuserdata(luavm, sender);
    lua_pushlightuserdata(luavm, item);
    lua_pcall(LuaVM, 2, 0, 0);
  finally
    lua_settop(Luavm, oldstack);
    luacs.leave;
  end
end;


//----------------------------Lua implementation-----------------------------
function LuaCaller_NotifyEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
begin
  result:=0;
  parameters:=lua_gettop(L);;

  if parameters=1 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));

    sender:=lua_touserdata(L, 1);
    lua_pop(L, lua_gettop(L));

    TNotifyEvent(m)(sender);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function LuaCaller_SelectionChangeEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
  user: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);;

  if parameters=1 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));

    sender:=lua_touserdata(L, 1);
    user:=lua_toboolean(L, 2);

    lua_pop(L, lua_gettop(L));

    TSelectionChangeEvent(m)(sender, user);
  end
  else
    lua_pop(L, lua_gettop(L));
end;


function LuaCaller_CloseEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
  closeaction: TCloseAction;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));
    sender:=lua_touserdata(L, 1);
    lua_pop(L, lua_gettop(L));

    TCloseEvent(m)(sender, closeaction);

    lua_pushinteger(L, integer(closeaction));
    result:=1;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function LuaCaller_MouseEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
  button: TMouseButton;
  shift: TShiftState;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=4 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));
    sender:=lua_touserdata(L, 1);
    button:=TMouseButton(lua_tointeger(L, 2));

    x:=lua_tointeger(L, 3);
    y:=lua_tointeger(L, 4);

    lua_pop(L, lua_gettop(L));

    TMouseEvent(m)(sender, button, [], x,y);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function LuaCaller_MouseMoveEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));
    sender:=lua_touserdata(L, 1);
    x:=lua_tointeger(L, 2);
    y:=lua_tointeger(L, 3);
    lua_pop(L, lua_gettop(L));

    TMouseMoveEvent(m)(sender, [],x,y);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function LuaCaller_KeyPressEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
  key: char;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));
    sender:=lua_touserdata(L, 1);
    s:=Lua_ToString(L,2);
    if length(s)>0 then
      key:=s[1]
    else
      key:=' ';

    lua_pop(L, lua_gettop(L));

    TKeyPressEvent(m)(sender, key);
    lua_pushstring(L, key);
    result:=1;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function LuaCaller_LVCheckedItemEvent(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  m: TMethod;
  sender: TObject;
  item: TListItem;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    m.code:=lua_touserdata(L, lua_upvalueindex(1));
    m.data:=lua_touserdata(L, lua_upvalueindex(2));
    sender:=lua_touserdata(L, 1);
    lua_pop(L, lua_gettop(L));

    TLVCheckedItemEvent(m)(sender,item);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

end.

