unit LuaOldD3DHook;

{$mode delphi}

interface

uses
  Classes, SysUtils, graphics, lua, lualib, lauxlib;

procedure initializeLuaOldD3DHook;

implementation

{$ifdef windows}

uses d3dhookUnit, LuaCaller, LuaHandler, LuaClass;

function d3dhook_getWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    d:=lua_toceuserdata(L,-parameters);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
    begin
      lua_pushinteger(L, d.getWidth);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

function d3dhook_getHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    d:=lua_toceuserdata(L,-parameters);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
    begin
      lua_pushinteger(L, d.getHeight);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

function d3dhook_initializeHook(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  size: integer;
  hookwindow: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
    size:=lua_tointeger(L, -parameters)
  else
    size:=16*1024*1024;

  if parameters>=2 then
    hookwindow:=lua_toboolean(L, -parameters+1)
  else
    hookwindow:=true;


  lua_pop(L, parameters);

  lua_pushboolean(L, safed3dhook(size, hookwindow)<>nil);
  result:=1;
end;

function d3dhook_onKey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  f: integer;
  routine: string;

  lc: TLuaCaller;
  m: TD3DKeyDownEvent;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if D3DHook<>nil then
    begin

      CleanupLuaCall(TMethod(d3dhook.onKeyDown));
      d3dhook.onKeyDown:=nil;

      if lua_isfunction(L,1) then
      begin
        f:=luaL_ref(L,LUA_REGISTRYINDEX);

        lc:=TLuaCaller.create;
        lc.luaroutineIndex:=f;
        d3dhook.onKeyDown:=lc.D3DKeyDownEvent;
      end
      else
      if lua_isstring(L,1) then
      begin
        routine:=lua_tostring(L,1);
        lc:=TLuaCaller.create;
        lc.luaroutine:=routine;
        d3dhook.onKeyDown:=lc.D3DKeyDownEvent;
      end;

    end;
  end;

  lua_pop(L, lua_gettop(L));
end;

function d3dhook_onClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if D3DHook<>nil then
    begin
      CleanupLuaCall(TMethod(d3dhook.onClick));
      d3dhook.onClick:=nil;

      if lua_isfunction(L,1) then
      begin
        f:=luaL_ref(L,LUA_REGISTRYINDEX);

        lc:=TLuaCaller.create;
        lc.luaroutineIndex:=f;
        d3dhook.OnClick:=lc.D3DClickEvent;
      end
      else
      if lua_isstring(L,1) then
      begin
        routine:=lua_tostring(L,1);
        lc:=TLuaCaller.create;
        lc.luaroutine:=routine;
        d3dhook.OnClick:=lc.D3DClickEvent;
      end;

    end;
  end;

  lua_pop(L, lua_gettop(L));
end;


function d3dhook_beginUpdate(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  d:=safed3dhook;
  if d<>nil then
  begin
    d.beginCommandListUpdate;
    d.beginTextureupdate;
  end;
end;

function d3dhook_endUpdate(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  d:=safed3dhook;
  if d<>nil then
  begin
    d.endTextureupdate;
    d.endCommandListUpdate;
  end;
end;


function d3dhook_setDisabledZBuffer(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    state:=lua_toboolean(L, -parameters);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setDisabledZBuffer(state);

  end else lua_pop(L, parameters);
end;

function d3dhook_setWireframeMode(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    state:=lua_toboolean(L, -parameters);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setWireframeMode(state);

  end else lua_pop(L, parameters);
end;


function d3dhook_setMouseClip(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    state:=lua_toboolean(L, -parameters);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setMouseClip(state);

  end else lua_pop(L, parameters);
end;

function d3dhook_enableConsole(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
  parameters: integer;
  key: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    key:=lua_tointeger(L, 1);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.enableConsole(key);

  end else lua_pop(L, parameters);
end;

function d3dhook_createTexture(L: PLua_State): integer; cdecl;
var parameters: integer;
  o: TObject;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    safed3dhook;

    if D3DHook<>nil then
    begin

      if lua_isuserdata(L,1) then
      begin
        //picture
        o:=lua_toceuserdata(L,1);
        lua_pop(L, parameters);

        if (o is TPicture) then
        begin
          luaclass_newClass(L, d3dhook.createTexture(tpicture(o)));
          result:=1;
        end;
      end
      else
      if lua_isstring(L,1) then
      begin
        //filename
        try
          s:=Lua_ToString(L,1);
          lua_pop(L, parameters);

          luaclass_newClass(L, d3dhook.createTexture(s));
          result:=1;
        except
        end;
      end;
    end
    else
      lua_pop(L, parameters);
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_texture_getHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  t: TD3DHook_Texture;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    t:=lua_toceuserdata(L,1);
    lua_pop(L, lua_gettop(L));
    lua_pushinteger(L, t.height);
    result:=1;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function d3dhook_texture_getWidth(L: PLua_State): integer; cdecl;
var
  t: TD3DHook_Texture;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    t:=lua_toceuserdata(L,1);

    lua_pop(L, lua_gettop(L));
    lua_pushinteger(L, t.width);
    result:=1;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function d3dhook_texture_loadTextureByPicture(L: PLua_State): integer; cdecl;
var
  t: TD3DHook_Texture;
  p: TPicture;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    t:=lua_toceuserdata(L,1);
    p:=lua_toceuserdata(L,2);
    lua_pop(L, lua_gettop(L));

    t.LoadTextureByPicture(p);
  end
  else
    lua_pop(L, lua_gettop(L));
end;


function d3dhook_createFontmap(L: PLua_State): integer; cdecl;
var parameters: integer;
  f: TFont;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    f:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    safed3dhook;

    if D3DHook<>nil then
    begin
      luaclass_newClass(L, d3dhook.createFontMap(f));
      result:=1;
    end;
  end
  else
    lua_pop(L, parameters);
end;



function d3dhook_fontmap_getTextWidth(L: PLua_State): integer; cdecl;
var parameters: integer;
  fm: TD3DHook_FontMap;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    fm:=lua_toceuserdata(L,1);
    s:=Lua_ToString(L,2);
    lua_pop(L, parameters);


    lua_pushinteger(L, fm.calculateFontWidth(s));
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function d3dhook_fontmap_changeFont(L: PLua_State): integer; cdecl;
var parameters: integer;
  f: TFont;
  fm: TD3DHook_FontMap;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    fm:=lua_toceuserdata(L,1);
    f:=lua_toceuserdata(L,2);
    lua_pop(L, parameters);

    fm.ChangeFont(f);
  end
  else
    lua_pop(L, parameters);
end;

function d3dhook_renderobject_getX(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushnumber(L, r.x);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_renderobject_setX(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
  v: single;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_tonumber(L,2);
    r.x:=v;
  end;

  lua_pop(L, parameters);
end;

function d3dhook_renderobject_getY(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushnumber(L, r.y);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_renderobject_setY(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
  v: single;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_tonumber(L,2);
    r.y:=v;
  end;

  lua_pop(L, parameters);
end;

function d3dhook_renderobject_getAlphablend(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushnumber(L, r.Alphablend);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_renderobject_setAlphablend(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
  v: single;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_tonumber(L,2);
    r.Alphablend:=v;
  end;

  lua_pop(L, parameters);
end;

function d3dhook_renderobject_getVisible(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushboolean(L, r.Visible);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_renderobject_setVisible(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
  v: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_toboolean(L,2);
    r.Visible:=v;
  end;

  lua_pop(L, parameters);
end;


function d3dhook_renderobject_getZOrder(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushinteger(L, r.ZOrder);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_renderobject_setZOrder(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_RenderObject;
  v: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_tointeger(L,2);
    r.ZOrder:=v;
  end;

  lua_pop(L, parameters);
end;

function d3dhook_createSprite(L: PLua_State): integer; cdecl;
var parameters: integer;
  t: TD3DHook_Texture;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    t:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    luaclass_newClass(L, d3dhook.createSprite(t));
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function d3dhook_sprite_getWidth(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_sprite;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushinteger(L, r.Width);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_sprite_setWidth(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_sprite;
  v: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_tointeger(L,2);
    r.Width:=v;
  end;

  lua_pop(L, parameters);
end;


function d3dhook_sprite_getHeight(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_sprite;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushinteger(L, r.Height);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_sprite_setHeight(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_sprite;
  v: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_tointeger(L,2);
    r.Height:=v;
  end;

  lua_pop(L, parameters);
end;


function d3dhook_sprite_getTexture(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_sprite;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    luaclass_newClass(L, r.Texture);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_sprite_setTexture(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_sprite;
  v: TD3DHook_Texture;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_toceuserdata(L,2);
    r.Texture:=v;
  end;

  lua_pop(L, parameters);
end;

function d3dhook_createTextContainer(L: PLua_State): integer; cdecl;
var parameters: integer;
  fm: TD3DHook_FontMap;
  x,y: single;
  text: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=4 then
  begin
    fm:=lua_toceuserdata(L,1);
    x:=lua_tonumber(L,2);
    y:=lua_tonumber(L,3);
    text:=Lua_ToString(L,4);
    lua_pop(L, parameters);

    luaclass_newClass(L, d3dhook.createTextContainer(fm,x,y,text));
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function d3dhook_TextContainer_getFontmap(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_TextContainer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    luaclass_newClass(L, r.Fontmap);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_TextContainer_setFontmap(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_TextContainer;
  v: TD3DHook_Fontmap;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=lua_toceuserdata(L,2);
    r.Fontmap:=v;
  end;

  lua_pop(L, parameters);
end;

function d3dhook_TextContainer_gettext(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_TextContainer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    r:=lua_toceuserdata(L,1);
    lua_pop(L, parameters);

    lua_pushstring(L, r.Text);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function d3dhook_TextContainer_settext(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: TD3DHook_TextContainer;
  v: string;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    r:=lua_toceuserdata(L,1);
    v:=Lua_ToString(L,2);
    r.text:=v;
  end;

  lua_pop(L, parameters);
end;

{$endif}

procedure initializeLuaOldD3DHook;
begin
  {$ifdef windows}
  lua_register(LuaVM, 'd3dhook_initializeHook', d3dhook_initializeHook);
  lua_register(LuaVM, 'd3dhook_onClick', d3dhook_onClick);
  lua_register(LuaVM, 'd3dhook_onKey', d3dhook_onKey);
  lua_register(LuaVM, 'd3dhook_beginUpdate', d3dhook_beginUpdate);
  lua_register(LuaVM, 'd3dhook_endUpdate', d3dhook_endUpdate);
  lua_register(LuaVM, 'd3dhook_getWidth', d3dhook_getWidth);
  lua_register(LuaVM, 'd3dhook_getHeight', d3dhook_getHeight);
  lua_register(LuaVM, 'd3dhook_setDisabledZBuffer', d3dhook_setDisabledZBuffer);
  lua_register(LuaVM, 'd3dhook_setWireframeMode', d3dhook_setWireframeMode);
  lua_register(LuaVM, 'd3dhook_setMouseClip', d3dhook_setMouseClip);
  lua_register(LuaVM, 'd3dhook_enableConsole', d3dhook_enableConsole);


  lua_register(LuaVM, 'd3dhook_createTexture', d3dhook_createTexture);
  lua_register(LuaVM, 'd3dhook_texture_getHeight', d3dhook_texture_getHeight);
  lua_register(LuaVM, 'd3dhook_texture_getWidth', d3dhook_texture_getWidth);
  lua_register(LuaVM, 'd3dhook_texture_loadTextureByPicture', d3dhook_texture_loadTextureByPicture);

  lua_register(LuaVM, 'd3dhook_createFontmap', d3dhook_createFontmap);
  lua_register(LuaVM, 'd3dhook_fontmap_changeFont', d3dhook_fontmap_changeFont);
  lua_register(LuaVM, 'd3dhook_fontmap_getTextWidth', d3dhook_fontmap_getTextWidth);


  lua_register(LuaVM, 'd3dhook_renderobject_getX', d3dhook_renderobject_getX);
  lua_register(LuaVM, 'd3dhook_renderobject_setX', d3dhook_renderobject_setX);
  lua_register(LuaVM, 'd3dhook_renderobject_getY', d3dhook_renderobject_getY);
  lua_register(LuaVM, 'd3dhook_renderobject_setY', d3dhook_renderobject_setY);
  lua_register(LuaVM, 'd3dhook_renderobject_getAlphablend', d3dhook_renderobject_getAlphablend);
  lua_register(LuaVM, 'd3dhook_renderobject_setAlphablend', d3dhook_renderobject_setAlphablend);
  lua_register(LuaVM, 'd3dhook_renderobject_getVisible', d3dhook_renderobject_getVisible);
  lua_register(LuaVM, 'd3dhook_renderobject_setVisible', d3dhook_renderobject_setVisible);
  lua_register(LuaVM, 'd3dhook_renderobject_getZOrder', d3dhook_renderobject_getZOrder);
  lua_register(LuaVM, 'd3dhook_renderobject_setZOrder', d3dhook_renderobject_setZOrder);

  lua_register(LuaVM, 'd3dhook_createSprite', d3dhook_createSprite);
  lua_register(LuaVM, 'd3dhook_sprite_getWidth', d3dhook_sprite_getWidth);
  lua_register(LuaVM, 'd3dhook_sprite_setWidth', d3dhook_sprite_setWidth);
  lua_register(LuaVM, 'd3dhook_sprite_getHeight', d3dhook_sprite_getheight);
  lua_register(LuaVM, 'd3dhook_sprite_setHeight', d3dhook_sprite_setheight);
  lua_register(LuaVM, 'd3dhook_sprite_getTexture', d3dhook_sprite_getTexture);
  lua_register(LuaVM, 'd3dhook_sprite_setTexture', d3dhook_sprite_setTexture);

  lua_register(LuaVM, 'd3dhook_createTextContainer', d3dhook_createTextContainer);
  lua_register(LuaVM, 'd3dhook_textcontainer_getFontMap', d3dhook_textcontainer_getFontMap);
  lua_register(LuaVM, 'd3dhook_textcontainer_setFontMap', d3dhook_textcontainer_setFontMap);
  lua_register(LuaVM, 'd3dhook_textcontainer_getText', d3dhook_textcontainer_getText);
  lua_register(LuaVM, 'd3dhook_textcontainer_setText', d3dhook_textcontainer_setText);
  {$endif}
end;


end.

