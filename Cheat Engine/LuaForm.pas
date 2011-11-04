unit LuaForm;

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, menus, lua, lualib, lauxlib, LuaHandler,
  LuaCaller, pluginexports, forms, dialogs, ceguicomponents, XMLWrite, XMLRead,
  Graphics, DOM, cefuncproc, newkernelhandler;

procedure initializeLuaForm;

implementation

function createForm(L: Plua_State): integer; cdecl;
var f: pointer;
  parameters: integer;
  visible: boolean;
begin
  result:=1;
  parameters:=lua_gettop(L);

  if parameters=1 then
    visible:=lua_toboolean(L,-1)
  else
    visible:=true;

  lua_pop(L, lua_gettop(L));


  f:=ce_createForm(visible);
  lua_pushlightuserdata(L, f);
  result:=1;
end;

function form_onClose(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomForm;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onClose));
    control.onClose:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClose:=lc.CloseEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClose:=lc.CloseEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function form_centerScreen(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    ce_form_centerScreen(f);
  end;
  lua_pop(L, lua_gettop(L));
end;

function form_hide(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    ce_form_hide(f);
  end;
  lua_pop(L, lua_gettop(L));
end;

function form_close(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: Tcustomform;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    f.Close;
  end;
  lua_pop(L, lua_gettop(L));
end;

function form_showModal(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: tcustomform;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));
    lua_pushinteger(L, f.ShowModal);
  end
  else
   lua_pop(L, lua_gettop(L));

end;


function form_isForegroundWindow(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: tcustomform;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));
    lua_pushboolean(L, GetForegroundWindow()=f.Handle);
    result:=1;
  end
  else
   lua_pop(L, lua_gettop(L));

end;

function form_getMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    form:=lua_touserdata(L,-1);
    lua_pop(L, parameters);


    lua_pushlightuserdata(L, form.menu);
    result:=1;

  end else lua_pop(L, parameters);
end;

function form_setMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
  menu: TMainmenu;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    form:=lua_touserdata(L,-2);
    menu:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    form.Menu:=menu;

  end else lua_pop(L, parameters);
end;


function form_getBorderstyle(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    form:=lua_touserdata(L,-1);
    lua_pop(L, parameters);


    lua_pushinteger(L, integer(form.Borderstyle));
    result:=1;

  end else lua_pop(L, parameters);
end;

function form_setBorderstyle(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
  Borderstyle: TBorderStyle;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    form:=lua_touserdata(L,-2);
    Borderstyle:=TBorderstyle(lua_tointeger(L,-1));
    lua_pop(L, parameters);

    form.Borderstyle:=Borderstyle;

  end else lua_pop(L, parameters);
end;


function form_show(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -1);
    ce_form_show(f);
  end;
  lua_pop(L, lua_gettop(L));
end;

function createFormFromFile(L: Plua_State): integer; cdecl;
var filename: string;
  f: TCEForm;
  formnode: TDOMNode;
  xmldoc: TXMLDocument;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    filename:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(L));

    try
      xmldoc:=nil;
      ReadXMLFile(xmldoc, filename);

      if xmldoc<>nil then
      begin
        formnode:=xmldoc.FindNode('FormData');
        f:=TCEForm.Create(application);
        f.LoadFromXML(formnode);
        f.ResyncWithLua;

        lua_pushlightuserdata(L, f);
        result:=1;
      end;
    except
      on e: exception do
      begin
        lua_pushstring(L, e.Message);
        lua_error(L);
      end;
    end;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function form_saveToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: TCEForm;
  filename: string;

  xmldoc: TXMLDocument;
  formnode: TDOMNode;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    f:=lua_touserdata(L, -parameters);
    filename:=Lua_ToString(L, -parameters+1);
    lua_pop(L, lua_gettop(L));

    if (f is TCEForm) then
    begin


      try
        xmldoc:=TXMLDocument.Create;

        formnode:=xmldoc.appendchild(xmldoc.createElement('FormData'));

        f.SaveCurrentStateasDesign;
        f.SaveToXML(formnode);

        WriteXML(xmldoc, filename);

        result:=1;
        lua_pushboolean(L, true);
      except
        on e: exception do
        begin
          lua_pushstring(L, e.Message);
          lua_error(L);
        end;
      end;
    end
    else
    begin
      lua_pushstring(L, 'The given form is not compatible. Formclass='+f.ClassName);
      lua_error(L);
    end;
  end
  else
    lua_pop(L, lua_gettop(L));

end;

function form_getDoNotSaveInTable(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: Tceform;
  align: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    form:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, form.DoNotSaveInTable);
    result:=1;

  end else lua_pop(L, parameters);
end;

function form_setDoNotSaveInTable(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: Tceform;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    form:=lua_touserdata(L,-2);
    form.DoNotSaveInTable:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;



function form_printToRasterImage(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: TCustomForm;
  ri: TRasterImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    f:=lua_touserdata(L, -2);
    ri:=lua_touserdata(L, -1);

    ri.Width:=f.ClientWidth;
    ri.Height:=f.ClientHeight;

    printwindow(f.handle, ri.Canvas.Handle, PW_CLIENTONLY);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function form_dragNow(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: TCustomForm;
  ri: TRasterImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_touserdata(L, -parameters);

    ReleaseCapture;
    SendMessageA(f.Handle,WM_SYSCOMMAND,$F012,0);
  end
  else
    lua_pop(L, lua_gettop(L));
end;

procedure initializeLuaForm;
begin
  lua_register(LuaVM, 'createForm', createForm);
  lua_register(LuaVM, 'createFormFromFile', createFormFromFile);

  lua_register(LuaVM, 'form_centerScreen', form_centerScreen);
  lua_register(LuaVM, 'form_onClose', form_onClose);
  lua_register(LuaVM, 'form_show', form_show);
  lua_register(LuaVM, 'form_hide', form_hide);
  lua_register(LuaVM, 'form_close', form_close);
  lua_register(LuaVM, 'form_showModal', form_showModal);
  lua_register(LuaVM, 'form_isForegroundWindow', form_isForegroundWindow);
  lua_register(LuaVM, 'form_getMenu', form_getMenu);
  lua_register(LuaVM, 'form_setMenu', form_setMenu);
  lua_register(LuaVM, 'form_saveToFile', form_saveToFile);
  lua_register(LuaVM, 'form_setDoNotSaveInTable', form_setDoNotSaveInTable);
  lua_register(LuaVM, 'form_getDoNotSaveInTable', form_getDoNotSaveInTable);
  lua_register(LuaVM, 'form_printToRasterImage', form_printToRasterImage);
  lua_register(LuaVM, 'form_dragNow', form_dragNow);
end;

end.

