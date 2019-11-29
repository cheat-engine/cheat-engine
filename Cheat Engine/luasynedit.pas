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

procedure luasynedit_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L, metatable, userdata);

end;

procedure initializeLuaSynEdit;
begin
  lua_register(LuaVM, 'createSynEdit', createSynEdit);
end;

initialization
  luaclass_register(TSynEdit, luasynedit_addMetaData);


end.

