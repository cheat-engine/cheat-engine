unit LuaEdit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Lua, Lualib, lauxlib;

procedure edit_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure initializeLuaEdit;

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

function edit_selectText(L: Plua_State): integer; cdecl;
var
  e: tcustomedit;
  idx: integer;
  paramc: integer;
  selStart: integer;
  selLength: integer;
  txtLength: integer;
begin
  e:=luaclass_getClassObject(L);
  paramc := lua_gettop(L);
  if paramc >= 1 then
  begin
    idx := 1;
    if not(lua_isnumber(L, idx)) then
       idx := 2;
    if (idx = 1) or lua_isnumber(L, idx) then
    begin
      selStart := lua_tointeger(L, idx);
      txtLength := e.GetTextLen;
      if selStart > txtLength then
      begin
         luaL_argerror(L, idx, 'the start of a selection cannot be bigger than the length of the text');
      end
      else
      begin
          if paramc >= 2 then
            selLength := lua_tointeger(L, idx+1)
          else
            selLength := txtLength - selStart;

          if (selStart + selLength) > txtLength then
             selLength := txtLength - selStart;

          e.SelStart := selStart;
          e.SelLength := selLength;
      end;
    end
    else
    begin
        luaL_argerror(L, idx, 'the first parameter must be a number!');
    end;
  end;
  result := 0;
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

function edit_getPasswordChar(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  lua_pushstring(L,e.PasswordChar);
  result:=1;
end;

function edit_setPasswordChar(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
  s: string;
begin
  e:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    s:=Lua_ToString(L,1);
    e.PasswordChar:=s[1];
  end;

  result:=1;
end;

function edit_getCaretPos(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  lua_pushpoint(L,e.CaretPos);
  result:=1;
end;

function edit_setCaretPos(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    e.caretpos:=lua_toPoint(L,1);

  result:=1;
end;

function edit_getSelStart(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  lua_pushinteger(L, e.SelStart);
  result:=1;
end;

function edit_setSelStart(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    e.SelStart:=lua_tointeger(L,1);

  result:=0;
end;

function edit_getSelLength(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  lua_pushinteger(L, e.SelLength);
  result:=1;
end;

function edit_setSelLength(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    e.SelLength:=lua_tointeger(L,1);
  result:=0;
end;

function edit_getSelText(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  lua_pushstring(L, e.SelText);
  result:=1;
end;

function edit_setSelText(L: PLua_State): integer; cdecl;
var
  e: tcustomedit;
begin
  e:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    e.SelText:=lua_tostring(L, 1);

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

function edit_getOnKeyPress(L: PLua_State): integer; cdecl;
var
  c: tcustomedit;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnKeyPress), 'TKeyPressEvent');
  result:=1;
end;

function edit_getOnKeyUp(L: PLua_State): integer; cdecl;
var
  c: tcustomedit;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnKeyUp), 'TKeyEvent');
  result:=1;
end;

function edit_getOnKeyDown(L: PLua_State): integer; cdecl;
var
  c: tcustomedit;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnKeyDown), 'TKeyEvent');
  result:=1;
end;

function edit_setOnKeyPress(L: PLua_State): integer; cdecl;
var
  control: TCustomEdit;
  f: integer;
  routine: string;
  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(control.OnKeyPress));
    control.OnKeyPress:=nil;

    if lua_isfunction(L, -1) then
    begin
      routine := Lua_ToString(L,-1);
      f := luaL_ref(L,LUA_REGISTRYINDEX);
      lc := TLuaCaller.create;
      lc.luaroutineIndex:= f;
      control.OnKeyPress:=lc.KeyPressEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnKeyPress:=lc.KeyPressEvent;
    end;
  end;
end;

function edit_setOnKeyUp(L: PLua_State): integer; cdecl;
var
  control: TCustomEdit;
  f: integer;
  routine: string;
  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(control.OnKeyUp));
    control.OnKeyUp:=nil;

    if lua_isfunction(L, -1) then
    begin
      routine := Lua_ToString(L,-1);
      f := luaL_ref(L,LUA_REGISTRYINDEX);
      lc := TLuaCaller.create;
      lc.luaroutineIndex:= f;
      control.OnKeyUp:=lc.KeyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnKeyUp:=lc.KeyEvent;
    end;
  end;
end;

function edit_setOnKeyDown(L: PLua_State): integer; cdecl;
var
  control: TCustomEdit;
  f: integer;
  routine: string;
  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(control.OnKeyDown));
    control.OnKeyDown:=nil;

    if lua_isfunction(L, -1) then
    begin
      routine := Lua_ToString(L,-1);
      f := luaL_ref(L,LUA_REGISTRYINDEX);
      lc := TLuaCaller.create;
      lc.luaroutineIndex:= f;
      control.OnKeyDown:=lc.KeyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnKeyDown:=lc.KeyEvent;
    end;
  end;
end;

procedure edit_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', edit_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'select', edit_selectText);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'selectAll', edit_selectAll);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'selectText', edit_selectText);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelText', edit_getSelText);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelStart', edit_getSelStart);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelLength', edit_getSelLength);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clearSelection', edit_clearSelection);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'copyToClipboard', edit_copyToClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'cutToClipboard', edit_cutToClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'pasteFromClipboard', edit_pasteFromClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnChange', edit_getOnChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnChange', edit_setOnChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnKeyPress', edit_getOnKeyPress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnKeyPress', edit_setOnKeyPress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnKeyUp', edit_getOnKeyUp);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnKeyUp', edit_setOnKeyUp);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnKeyDown', edit_getOnKeyDown);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnKeyDown', edit_setOnKeyDown);


  luaclass_addPropertyToTable(L, metatable, userdata, 'SetPasswordChar', edit_setPasswordChar, edit_getPasswordChar);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CaretPos', edit_getCaretPos, edit_setCaretPos);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelStart', edit_getSelStart, edit_setSelStart);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelLength', edit_getSelLength, edit_setSelLength);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelText', edit_getSelText, edit_setSelText);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnChange', edit_getOnChange, edit_setOnChange);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnKeyPress', edit_getOnKeyPress, edit_setOnKeyPress);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnKeyUp', edit_getOnKeyUp, edit_setOnKeyUp);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnKeyDown', edit_getOnKeyDown, edit_setOnKeyDown);
end;

procedure initializeLuaEdit;
begin
  lua_register(LuaVM, 'createEdit', createEdit);
  lua_register(LuaVM, 'edit_clear', edit_clear);
  lua_register(LuaVM, 'edit_getSelStart', edit_getSelStart);
  lua_register(LuaVM, 'edit_getSelLength', edit_getSelLength);
  lua_register(LuaVM, 'edit_getSelText', edit_getSelText);
  lua_register(LuaVM, 'edit_select', edit_selectText);
  lua_register(LuaVM, 'edit_selectAll', edit_selectAll);
  lua_register(LuaVM, 'edit_selectText', edit_selectText);
  lua_register(LuaVM, 'edit_clearSelection', edit_clearSelection);
  lua_register(LuaVM, 'edit_copyToClipboard', edit_copyToClipboard);
  lua_register(LuaVM, 'edit_cutToClipboard', edit_cutToClipboard);
  lua_register(LuaVM, 'edit_pasteFromClipboard', edit_pasteFromClipboard);
  lua_register(LuaVM, 'edit_onChange', edit_setOnChange);
  lua_register(LuaVM, 'edit_getOnChange', edit_getOnChange);
  lua_register(LuaVM, 'edit_setOnChange', edit_setOnChange);
  lua_register(LuaVM, 'edit_onKeyPress', edit_setOnKeyPress);
  lua_register(LuaVM, 'edit_getOnKeyPress', edit_getOnKeyPress);
  lua_register(LuaVM, 'edit_setOnKeyPress', edit_setOnKeyPress);
  lua_register(LuaVM, 'edit_onKeyUp', edit_setOnKeyUp);
  lua_register(LuaVM, 'edit_getOnKeyUp', edit_getOnKeyUp);
  lua_register(LuaVM, 'edit_setOnKeyUp', edit_setOnKeyUp);
  lua_register(LuaVM, 'edit_onKeyDown', edit_setOnKeyDown);
  lua_register(LuaVM, 'edit_getOnKeyDown', edit_getOnKeyDown);
  lua_register(LuaVM, 'edit_setOnKeyDown', edit_setOnKeyDown);
end;

initialization
  luaclass_register(TCustomEdit, edit_addMetaData);

end.

