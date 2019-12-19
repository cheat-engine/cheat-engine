unit LuaMemo;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lua, Lualib, lauxlib, StdCtrls, controls;

procedure initializeLuaMemo;

implementation

uses luaclass, luahandler, LuaWinControl, LuaEdit;

function createMemo(L: Plua_State): integer; cdecl;
var
  m: tmemo;
  o: twincontrol;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    o:=lua_toceuserdata(L, -1);
    m:=tmemo.create(o);
    m.parent:=o;

    luaclass_newClass(L, m);
    result:=1;
  end;
end;

function memo_append(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    memo.Append(Lua_ToString(L,-1));

  result:=0;
end;

function memo_getLines(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  luaclass_newClass(L, memo.Lines);
  result:=1;
end;

function memo_setLines(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    memo.lines:=tstrings(lua_ToCEUserData(L, 1));

  result:=0;
end;

function memo_getWordWrap(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  lua_pushvariant(L, memo.WordWrap);
  result:=1;
end;

function memo_setWordWrap(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memo.WordWrap:=lua_tovariant(L, -1);
  result:=0;
end;

function memo_getWantTabs(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  lua_pushvariant(L, memo.WantTabs);
  result:=1;
end;

function memo_setWantTabs(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memo.WantTabs:=lua_tovariant(L, -1);
  result:=0;
end;

function memo_getWantReturns(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  lua_pushvariant(L, memo.WantReturns);
  result:=1;
end;

function memo_setWantReturns(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memo.WantReturns:=lua_tovariant(L, -1);
  result:=0;
end;

function memo_getScrollBars(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(memo.ScrollBars));
  result:=1;
end;

function memo_setScrollBars(L: PLua_State): integer; cdecl;
var
  memo: TCustomMemo;
begin
  memo:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memo.ScrollBars:=TScrollStyle(lua_tointeger(L, -1));
  result:=0;
end;

procedure memo_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  edit_addMetaData(L, metatable, userdata);   //inherits from customedit

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'append', memo_append);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getLines', memo_getLines);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setLines', memo_setLines);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWordWrap', memo_getWordWrap);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWordWrap', memo_setWordWrap);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWantTabs', memo_getWantTabs);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWantTabs', memo_setWantTabs);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWantReturns', memo_getWantReturns);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWantReturns', memo_setWantReturns);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getScrollbars', memo_getScrollbars);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setScrollbars', memo_setScrollbars);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Lines', memo_getLines, memo_setLines);
  luaclass_addPropertyToTable(L, metatable, userdata, 'WordWrap', memo_getWordWrap, memo_setWordWrap);
  luaclass_addPropertyToTable(L, metatable, userdata, 'WantTabs', memo_getWantTabs, memo_setWantTabs);
  luaclass_addPropertyToTable(L, metatable, userdata, 'WantReturns', memo_getWantReturns, memo_setWantReturns);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Scrollbars', memo_getScrollbars, memo_setScrollbars);
end;

procedure initializeLuaMemo;
begin
  lua_register(LuaVM, 'createMemo', createMemo);
  lua_register(LuaVM, 'memo_append', memo_append);
  lua_register(LuaVM, 'memo_getLines', memo_getLines);
  lua_register(LuaVM, 'memo_getWordWrap', memo_getWordWrap);
  lua_register(LuaVM, 'memo_setWordWrap', memo_setWordWrap);
  lua_register(LuaVM, 'memo_getWantTabs', memo_getWantTabs);
  lua_register(LuaVM, 'memo_setWantTabs', memo_setWantTabs);
  lua_register(LuaVM, 'memo_getWantReturns', memo_getWantReturns);
  lua_register(LuaVM, 'memo_setWantReturns', memo_setWantReturns);
  lua_register(LuaVM, 'memo_getScrollbars', memo_getScrollbars);
  lua_register(LuaVM, 'memo_setScrollbars', memo_setScrollbars);
end;

initialization
  luaclass_register(TCustomMemo, memo_addMetaData);

end.

