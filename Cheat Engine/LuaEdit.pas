unit LuaEdit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Lua, Lualib, lauxlib;

implementation

uses luahandler, LuaClass, LuaCaller, LuaWinControl;

function createEdit(L: Plua_State): integer; cdecl;
var
  e: Tedit;
  w: twincontrol;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    w:=lua_toceuserdata(L, -1);
    e:=tedit.create(w);
    e.parent:=w;
    luaclass_newClass(L, e);
    result:=1;
  end;
end;

function edit_clear(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  e.clear;
  result:=0;
end;

function edit_selectAll(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  e.SelectAll;
  result:=0;
end;

function edit_clearSelection(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  e.ClearSelection;
  result:=0;
end;

function edit_copyToClipboard(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  e.CopyToClipboard;
  result:=0;
end;

function edit_cutToClipboard(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  e.CutToClipboard;
  result:=0;
end;

function edit_pasteFromClipboard(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  e.PasteFromClipboard;
  result:=0;
end;

function edit_getOnChange(L: PLua_State): integer; cdecl;
var
  c: tcustomedit;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnChange), 'TNotifyEvent');
  result:=1;
end;

function edit_setonChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomEdit;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(control.onChange));
    control.onChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnChange:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnChange:=lc.NotifyEvent;
    end;

  end;
end;

procedure edit_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', edit_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'selectAll', edit_selectAll);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clearSelection', edit_clearSelection);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'copyToClipboard', edit_copyToClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'cutToClipboard', edit_cutToClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'pasteFromClipboard', edit_pasteFromClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnChange', edit_getOnChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnChange', edit_setOnChange);

  luaclass_addPropertyToTable(L, metatable, userdata, 'OnChange', edit_getOnChange, edit_setOnChange);
end;

procedure initializeLuaEdit;
begin
  lua_register(LuaVM, 'createEdit', createEdit);
  lua_register(LuaVM, 'edit_clear', edit_clear);
  lua_register(LuaVM, 'edit_selectAll', edit_selectAll);
  lua_register(LuaVM, 'edit_clearSelection', edit_clearSelection);
  lua_register(LuaVM, 'edit_copyToClipboard', edit_copyToClipboard);
  lua_register(LuaVM, 'edit_cutToClipboard', edit_cutToClipboard);
  lua_register(LuaVM, 'edit_pasteFromClipboard', edit_pasteFromClipboard);
  lua_register(LuaVM, 'edit_onChange', edit_onChange);
end;

initialization
  luaclass_register(TCustomEdit, edit_addMetaData);

end.

