unit LuaD3DHook;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, Graphics;

procedure initializeLuaD3dHook;

implementation

{$ifdef windows}
uses luaclass, luahandler, d3dhookUnit, LuaObject;

function createD3DHook(L: PLua_State): integer; cdecl;
var
  size: integer;
  hookwindow: boolean;
begin

  size:=16*1024*1024;
  hookwindow:=true;

  if lua_gettop(L)=1 then
    size:=lua_tointeger(L, 1);

  if lua_gettop(L)=2 then
    hookwindow:=lua_toboolean(L, 2);

  luaclass_newClass(L, safed3dhook(size, hookwindow));
  result:=1;
end;

function d3dhook_beginupdate(L: PLua_State): integer; cdecl;
var d: TD3DHook;
begin
  d:=luaclass_getClassObject(L);
  d.beginCommandListUpdate;
  d.beginTextureUpdate;
  result:=0;
end;

function d3dhook_endUpdate(L: PLua_State): integer; cdecl;
var d: TD3DHook;
begin
  d:=luaclass_getClassObject(L);
  d.endTextureUpdate;
  d.endCommandListUpdate;

  result:=0;
end;


function d3dhook_enableConsole(L: PLua_State): integer; cdecl;
var d: TD3DHook;
    k: dword;
begin
  d:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    k:=lua_tointeger(L, 1)
  else
    k:=$c0; //~

  d.enableConsole(k);

  result:=0;
end;

function d3dhook_createTexture(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
  o: TObject;
  s: string;

  p: TPicture;
begin
  d:=luaclass_getClassObject(L);
  result:=0;
  if lua_gettop(L)>=1 then
  begin

    if lua_isuserdata(L,1) then
    begin
      //picture
      o:=lua_toceuserdata(L,1);
      if (o is TPicture) then
      begin
        if lua_gettop(L)=2 then //transparency color given
        begin
          //create a new picture with 32-bit color and change the transparent color
          p:=tpicture.create;
          p.png.PixelFormat:=pf32bit;
          p.png.Transparent:=true;
          p.png.TransparentColor:=lua_tointeger(L, 2);
          p.png.width:=tpicture(o).Width;
          p.png.Height:=tpicture(o).height;

          p.png.canvas.CopyRect(rect(0,0,tpicture(o).Width,tpicture(o).Height), tpicture(o).bitmap.canvas, rect(0,0,tpicture(o).Width,tpicture(o).Height));

          FixAlpha(p.png);
          luaclass_newClass(L, d.createTexture(p));
          p.free;
        end
        else
          luaclass_newClass(L, d.createTexture(tpicture(o)));
        result:=1;
      end;
    end
    else
    if lua_isstring(L,1) then
    begin
      //filename
      try
        s:=Lua_ToString(L,1);
        luaclass_newClass(L, d.createTexture(s));
        result:=1;
      except
      end;
    end;

  end
end;

function d3dhook_createFontmap(L: PLua_State): integer; cdecl;
var
  f: TFont;
  s: string;
  d: TD3DHook;
begin
  result:=0;

  d:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    f:=lua_toceuserdata(L,1);
    luaclass_newClass(L, d.createFontMap(f));
    result:=1;
  end;
end;

function d3dhook_createSprite(L: PLua_State): integer; cdecl;
var parameters: integer;
  t: TD3DHook_Texture;
  s: string;
  d: TD3DHook;
begin
  result:=0;
  d:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    t:=lua_toceuserdata(L,1);
    luaclass_newClass(L, d.createSprite(t));
    result:=1;
  end;
end;

function d3dhook_createTextContainer(L: PLua_State): integer; cdecl;
var
  fm: TD3DHook_FontMap;
  x,y: single;
  text: string;
  d: TD3DHook;
begin
  result:=0;
  d:=luaclass_getClassObject(L);

  if lua_gettop(L)=4 then
  begin
    fm:=lua_toceuserdata(L,1);
    x:=lua_tonumber(L,2);
    y:=lua_tonumber(L,3);
    text:=Lua_ToString(L,4);
    luaclass_newClass(L, d3dhook.createTextContainer(fm,x,y,text));
    result:=1;
  end;
end;


procedure d3dhook_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', d3dhook_beginupdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', d3dhook_endUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'enableConsole', d3dhook_enableConsole);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createTexture', d3dhook_createTexture);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createFontmap', d3dhook_createFontmap);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createTextContainer', d3dhook_createTextContainer);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createSprite', d3dhook_createSprite);


end;

function d3dhook_texture_loadTextureByPicture(L: PLua_State): integer; cdecl;
var t: TD3DHook_Texture;
  p: tpicture;
begin
  result:=0;
  t:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p:=lua_ToCEUserData(L, 1);
    t.LoadTextureByPicture(p);
  end;
end;

procedure d3dhook_texture_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadTextureByPicture', d3dhook_texture_loadTextureByPicture);
end;


function d3dhook_fontmap_getTextWidth(L: PLua_State): integer; cdecl;
var
  fm: TD3DHook_FontMap;
  s: string;
begin
  result:=0;
  fm:=luaclass_getClassObject(L);


  if lua_gettop(L)=1 then
  begin
    s:=Lua_ToString(L,1);
    lua_pushinteger(L, fm.calculateFontWidth(s));
    result:=1;
  end
end;

function d3dhook_fontmap_changeFont(L: PLua_State): integer; cdecl;
var
  f: TFont;
  fm: TD3DHook_FontMap;
  s: string;
begin
  result:=0;
  fm:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    f:=lua_toceuserdata(L,1);
    fm.ChangeFont(f);
  end;
end;

procedure d3dhook_fontmap_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  d3dhook_texture_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'changeFont', d3dhook_fontmap_changeFont);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTextWidth', d3dhook_fontmap_getTextWidth);
end;

//renderobject has only publised properties and inherits from object, no need to define
//same for D3DHook_Sprite and textcontainer

{$endif}


procedure initializeLuaD3dHook;
begin
  {$ifdef windows}
  lua_register(LuaVM, 'createD3DHook', createD3DHook);
  {$endif}

end;

initialization
  {$ifdef windows}
  luaclass_register(TD3DHook, d3dhook_addMetaData);
  luaclass_register(TD3DHook_Texture, d3dhook_texture_addMetaData);
  luaclass_register(TD3DHook_FontMap, d3dhook_fontmap_addMetaData);
  {$endif}


end.

