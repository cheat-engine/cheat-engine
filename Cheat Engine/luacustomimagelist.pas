unit LuaCustomImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, lua, lualib, lauxlib, ImgList;

procedure initializeLuaCustomImageList;

implementation

uses luahandler, luaclass, LuaComponent, LuaCaller, Graphics, Forms;

function createImageList(L: Plua_State): integer; cdecl;
var il: TImageList;
  owner: TComponent;
begin
  if lua_gettop(L)>=1 then
    owner:=lua_ToCEUserData(L,1)
  else
    owner:=nil;

  il:=TImageList.Create(owner);
  luaclass_newClass(L, il);
  result:=1;
end;

function customimagelist_draw(L: PLua_State): integer; cdecl;
var
  c: TCustomImageList;
  canvas: tcanvas;
  x,y: integer;
  index: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L);

  if lua_gettop(L)>=4 then
  begin
    canvas:=lua_ToCEUserData(L,1);
    x:=lua_tointeger(L,2);
    y:=lua_tointeger(L,3);
    index:=lua_tointeger(L,4);

    c.DrawForPPI(canvas,x,y,index,c.width,screen.PixelsPerInch,1);
  end;

end;

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
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'draw', customimagelist_draw);



  luaclass_addPropertyToTable(L, metatable, userdata,'Count',customimagelist_getCount,nil);
end;

procedure initializeLuaCustomImageList;
begin
  lua_register(LuaVM, 'createImageList', createImageList);
end;

initialization
  luaclass_register(TCustomImageList, customimagelist_addMetaData);


end.

