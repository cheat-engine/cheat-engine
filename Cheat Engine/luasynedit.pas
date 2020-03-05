unit LuaSynedit;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure initializeLuaSynEdit;

implementation

uses lua, lauxlib, lualib, luahandler, luaclass, controls, SynEdit, LuaCustomControl, LuaSyntax, SynHighlighterAA;

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


procedure luasynedit_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'SelStart', syn_getSelStart, syn_setSelStart);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelEnd', syn_getSelEnd, syn_setSelEnd);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelText', syn_getSelText, syn_setSelText);

end;

procedure initializeLuaSynEdit;
begin
  lua_register(LuaVM, 'createSynEdit', createSynEdit);
end;

initialization
  luaclass_register(TSynEdit, luasynedit_addMetaData);


end.

