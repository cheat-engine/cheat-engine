unit LuaSynedit;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure initializeLuaSynEdit;

implementation

uses lua, lauxlib, lualib, luahandler, luaclass, controls, SynEdit, LuaCustomControl, LuaSyntax, SynHighlighterAA, betterControls;

function createSynEdit(L: PLua_State): integer; cdecl;
var
  s: TSynEdit;
  o: twincontrol;

  mode: integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    o:=lua_toceuserdata(L, 1);
    s:=TSynEdit.Create(o);
    s.parent:=o;

    if lua_gettop(L)>=2 then
    begin
      mode:=lua_tointeger(L,2);

      case mode of
        0: s.Highlighter:=TSynLuaSyn.Create(s);
        1: s.Highlighter:=TSynAASyn.Create(s);
      end;
    end;

    luaclass_newClass(L, s);
    result:=1;


  end;
end;


function syn_getSelStart(L: PLua_State): integer; cdecl;
var
  s: TCustomSynEdit;
begin
  s:=luaclass_getClassObject(L);
  lua_pushinteger(L, s.SelStart);
  result:=1;
end;

function syn_setSelStart(L: PLua_State): integer; cdecl;
var
  s: TCustomSynEdit;
begin
  s:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    s.SelStart:=lua_tointeger(L,1);

  result:=0;
end;

function syn_getSelEnd(L: PLua_State): integer; cdecl;
var
  s: TCustomSynEdit;
begin
  s:=luaclass_getClassObject(L);
  lua_pushinteger(L, s.SelEnd);
  result:=1;
end;

function syn_setSelEnd(L: PLua_State): integer; cdecl;
var
  s: TCustomSynEdit;
begin
  s:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    s.SelEnd:=lua_tointeger(L,1);
  result:=0;


end;

function syn_getSelText(L: PLua_State): integer; cdecl;
var
  s: TCustomSynEdit;
begin
  s:=luaclass_getClassObject(L);
  lua_pushstring(L, s.SelText);
  result:=1;
end;

function syn_setSelText(L: PLua_State): integer; cdecl;
var
  s: TCustomSynEdit;
begin
  s:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    s.SelText:=lua_tostring(L, 1);


  result:=0;
end;

function syn_getCanPaste(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, TCustomSynEdit(luaclass_getClassObject(L)).CanPaste);
  result:=1;
end;

function syn_getCanRedo(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, TCustomSynEdit(luaclass_getClassObject(L)).CanRedo);
  result:=1;
end;

function syn_getCanUndo(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, TCustomSynEdit(luaclass_getClassObject(L)).CanUndo);
  result:=1;
end;

function syn_CopyToClipboard(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).CopyToClipboard;
  result:=0;
end;

function syn_CutToClipboard(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).CutToClipboard;
  result:=0;
end;

function syn_PasteFromClipboard(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).PasteFromClipboard;
  result:=0;
end;

function syn_ClearUndo(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).ClearUndo;
  result:=0;
end;

function syn_Redo(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).Redo;
  result:=0;
end;

function syn_Undo(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).Undo;
  result:=0;
end;

function syn_MarkTextAsSaved(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).MarkTextAsSaved;
  result:=0;
end;

function syn_ClearSelection(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).ClearSelection;
  result:=0;
end;

function syn_SelectAll(L: PLua_State): integer; cdecl;
begin
  TCustomSynEdit(luaclass_getClassObject(L)).SelectAll;
  result:=0;
end;

function syn_getCharWidth(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L, TCustomSynEdit(luaclass_getClassObject(L)).CharWidth);
  result:=1;
end;

function syn_getLineHeight(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L, TCustomSynEdit(luaclass_getClassObject(L)).LineHeight);
  result:=1;
end;

procedure luasynedit_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'SelStart', syn_getSelStart, syn_setSelStart);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelEnd', syn_getSelEnd, syn_setSelEnd);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelText', syn_getSelText, syn_setSelText);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CanPaste', syn_getCanPaste, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CanRedo', syn_getCanRedo, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CanUndo', syn_getCanUndo, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CharWidth', syn_getCharWidth, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'LineHeight', syn_getLineHeight, nil);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'CopyToClipboard', syn_CopyToClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'CutToClipboard', syn_CutToClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'PasteFromClipboard', syn_PasteFromClipboard);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'ClearUndo', syn_ClearUndo);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'Redo', syn_Redo);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'Undo', syn_Undo);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'MarkTextAsSaved', syn_MarkTextAsSaved);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'ClearSelection', syn_ClearSelection);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'SelectAll', syn_SelectAll);
end;

procedure initializeLuaSynEdit;
begin
  lua_register(LuaVM, 'createSynEdit', createSynEdit);
end;

initialization
  luaclass_register(TSynEdit, luasynedit_addMetaData);


end.

