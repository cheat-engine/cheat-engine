unit LuaCustomImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, lua, lualib, lauxlib, ImgList;

implementation

uses luahandler, luaclass, LuaComponent, LuaCaller, Graphics;


function customimagelist_add(L: PLua_State): integer; cdecl;
var
  c: TCustomImageList;
  i: integer;
  img, mask: TCustomBitmap;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_Gettop(L)>=1 then
  begin
    img:=lua_ToCEUserData(L,1);

    if lua_Gettop(L)>=2 then
      mask:=lua_ToCEUserData(L,2)
    else
      mask:=nil;

    lua_pushinteger(L, c.Add(img,mask));
    result:=1;
  end;
end;

function customimagelist_getCount(L: PLua_State): integer; cdecl;
var
  c: TCustomImageList;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L,c.count);
  result:=1;
end;




procedure customimagelist_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', customimagelist_add);
  luaclass_addPropertyToTable(L, metatable, userdata,'Count',customimagelist_getCount,nil);
end;

initialization
  luaclass_register(TCustomImageList, customimagelist_addMetaData);


end.

