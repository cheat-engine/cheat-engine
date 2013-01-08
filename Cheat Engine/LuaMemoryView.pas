unit LuaMemoryView;

{$mode delphi}

interface

uses
  forms, Classes, SysUtils, lua;

procedure initializeLuaMemoryview;

implementation

uses luahandler, luaclass, MemoryBrowserFormUnit;

function createMemoryView(L: PLua_state): integer; cdecl;
begin
  luaclass_newClass(L, TMemoryBrowser.Create(application));
  result:=1;
end;

function memoryview_getHexadecimalView(L: PLua_state): integer; cdecl;
var m: TMemoryBrowser;
begin
  m:=luaclass_getClassObject(L);
  luaclass_newClass(L, m.hexview);
  result:=1;
end;

function memoryview_getDisassemblerView(L: PLua_state): integer; cdecl;
var m: TMemoryBrowser;
begin
  m:=luaclass_getClassObject(L);
  luaclass_newClass(L, m.disassemblerview);
  result:=1;
end;


procedure initializeLuaMemoryView;
begin
  lua_register(LuaVM, 'createMemoryView', createMemoryView);
  lua_register(LuaVM, 'memoryview_getDisassemblerView', memoryview_getDisassemblerView);
  lua_register(LuaVM, 'memoryview_getHexadecimalView', memoryview_getHexadecimalView);

end;

end.


