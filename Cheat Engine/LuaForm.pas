unit LuaForm;

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils, Controls, menus, lua, lualib, lauxlib, LuaHandler,
  LuaCaller, pluginexports, forms, dialogs, ceguicomponents, XMLWrite, XMLRead,
  Graphics, DOM, cefuncproc, newkernelhandler, typinfo;

procedure initializeLuaForm;

procedure customForm_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luaclass, LuaCustomControl;

resourcestring
  rsTheGivenFormIsNotCompatible = 'The given form is not compatible. Formclass=';

function createForm(L: Plua_State): integer; cdecl;
var f: tcustomform;
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

  f:=ce_createForm(visible);  //not relly a customform, but it inherits from it, so good enough
  f.PopupMode:=pmAuto;
  luaclass_newClass(L, f);
  result:=1;

end;

function customform_getOnClose(L: PLua_State): integer; cdecl;
var
  c: TCustomForm;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnClose), 'TCloseEvent');
  result:=1;
end;

function customform_setOnClose(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TCustomForm;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  control:=luaclass_getClassObject(L);
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(control.onClose));
    control.onClose:=nil;

    if lua_isfunction(L,1) then
    begin
      routine:=Lua_ToString(L,1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClose:=lc.CloseEvent;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClose:=lc.CloseEvent;
    end;

  end;
end;

function customform_centerScreen(L: Plua_State): integer; cdecl;
var
  f: TCustomForm;
begin
  f:=luaclass_getClassObject(L);
  f.Position:=poScreenCenter;
  result:=0;
end;

function customform_hide(L: Plua_State): integer; cdecl;
var
  f: TCustomForm;
begin
  f:=luaclass_getClassObject(L);
  f.Hide;
  result:=0;
end;

function customform_close(L: Plua_State): integer; cdecl;
var
  f: Tcustomform;
begin
  result:=0;
  f:=luaclass_getClassObject(L);
  f.close;
end;

function customform_bringToFront(L: Plua_State): integer; cdecl;
var
  f: Tcustomform;
begin
  result:=0;
  f:=luaclass_getClassObject(L);
  f.BringToFront;
end;

function customform_showModal(L: Plua_State): integer; cdecl;
var
  f: tcustomform;
begin
  result:=1;
  f:=luaclass_getClassObject(L);
  lua_pushinteger(L, f.ShowModal);
end;


function customform_isForegroundWindow(L: Plua_State): integer; cdecl;
var
  f: tcustomform;
begin
  result:=1;
  f:=luaclass_getClassObject(L);
  lua_pushboolean(L, GetForegroundWindow()=f.Handle);
end;

function customform_getMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  form: TCustomForm;
begin
  result:=1;
  form:=luaclass_getClassObject(L);
  luaclass_newClass(L, form.menu);
end;

function customform_setMenu(L: PLua_State): integer; cdecl;
var
  form: TCustomForm;
  menu: TMainmenu;
begin
  result:=0;
  form:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    menu:=lua_ToCEUserData(L,-1);
    form.Menu:=menu;
  end;
end;


function customform_getBorderstyle(L: PLua_State): integer; cdecl;
var
  form: TCustomForm;
begin
  form:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(form.Borderstyle));
  result:=1;
end;

function customform_setBorderstyle(L: PLua_State): integer; cdecl;
var
  form: TCustomForm;
  Borderstyle: TBorderStyle;
begin
  result:=0;
  form:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    Borderstyle:=TBorderstyle(lua_tointeger(L,-1));
    form.Borderstyle:=Borderstyle;
  end;
end;


function customform_show(L: Plua_State): integer; cdecl;
var
  f: TCustomForm;
begin
  result:=0;
  f:=luaclass_getClassObject(L);
  f.Show;
end;

function customform_printToRasterImage(L: Plua_State): integer; cdecl;
var
  f: TCustomForm;
  ri: TRasterImage;
begin
  result:=0;
  f:=luaclass_getClassObject(L);


  if lua_gettop(L)>=1 then
  begin
    ri:=lua_toceuserdata(L, -1);

    ri.Width:=f.ClientWidth;
    ri.Height:=f.ClientHeight;

    printwindow(f.handle, ri.Canvas.Handle, PW_CLIENTONLY);
  end;
end;

function customform_dragNow(L: Plua_State): integer; cdecl;
var
  f: TCustomForm;
begin
  result:=0;
  f:=luaclass_getClassObject(L);
  ReleaseCapture;
  SendMessageA(f.Handle,WM_SYSCOMMAND,$F012,0);
end;


function createFormFromFile(L: Plua_State): integer; cdecl;
var filename: string;
  f: TCEForm;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    filename:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(L));

    f:=TCEForm.Createnew(nil);   //6.3: was application
    f.LoadFromFile(filename);

    luaclass_newClass(L, f);
    result:=1;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function ceform_saveToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: TCEForm;
  filename: string;
begin
  result:=0;
  f:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    filename:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(L));

    if (f is TCEForm) then
    begin
      try
        f.SaveToFile(filename);
        //no errors

        lua_pushboolean(L, true);
        result:=1;
      except
        on e: exception do
        begin
          lua_pushstring(L, e.Message);
          lua_error(L);
        end;
      end;
    end
    else
      raise exception.create(rsTheGivenFormIsNotCompatible+f.ClassName);
  end
  else
    lua_pop(L, lua_gettop(L));

end;

function ceform_getDoNotSaveInTable(L: PLua_State): integer; cdecl;
var
  form: Tceform;
begin
  form:=luaclass_getClassObject(L);
  lua_pushboolean(L, form.DoNotSaveInTable);
  result:=1;
end;

function ceform_setDoNotSaveInTable(L: PLua_State): integer; cdecl;
var
  form: Tceform;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    form.DoNotSaveInTable:=lua_toboolean(L,-1);
end;

function ceform_saveCurrentStateAsDesign(L: PLua_State): integer; cdecl;
var
  form: Tceform;
begin
  result:=0;
  form:=luaclass_getClassObject(L);
  form.SaveCurrentStateasDesign;
end;

function customform_getFormState(L: PLua_State): integer; cdecl;
var
  form: TCustomForm;
  ti: PTypeInfo;
begin
  form:=luaclass_getClassObject(L);

  ti:=typeinfo(TFormState);
  lua_pushstring(L, SetToString(ti, integer(form.FormState),true));
  result:=1;
end;

function customform_getModalResult(L: PLua_State): integer; cdecl;
var
  form: TCustomForm;
begin
  form:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(form.ModalResult));
  result:=1;
end;

function customform_setModalResult(L: PLua_State): integer; cdecl;
var
  form: TCustomForm;
  ModalResult: TModalResult;
begin
  result:=0;
  form:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    ModalResult:=TModalResult(lua_tointeger(L,1));
    form.ModalResult:=ModalResult;
  end;
end;


function customform_unregisterCreateCallback(L: PLua_State): integer; cdecl;
var
  lc: TLuacaller;
  form: TCustomForm;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    lc:=lua_ToCEUserData(L, -1);
    if lc<>nil then
      form.RemoveHandlerCreate(lc.NotifyEvent);

    lc.Free;
  end;
end;

function customform_registerCreateCallback(L: PLua_State): integer; cdecl;
var lc: TLuaCaller;
  f: integer;
  routine: string;
  form: TCustomForm;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    lc:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      form.AddHandlerCreate(lc.NotifyEvent);
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      form.AddHandlerCreate(lc.NotifyEvent);
    end;

    luaclass_newClass(L, lc);
    result:=1;
  end;
end;


function customform_unregisterFirstShowCallback(L: PLua_State): integer; cdecl;
var
  lc: TLuacaller;
  form: TCustomForm;

  m: TMethod;
  nr: TNotifyEvent;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    lc:=lua_ToCEUserData(L, -1);
    if lc<>nil then
    begin
      nr:=lc.NotifyEvent;
      m:=Tmethod(nr);
      if tobject(m.data) is TLuaCaller then
      begin
        form.RemoveHandlerFirstShow(nr);
        lc.Free;
      end;
    end;


  end;
end;

function customform_registerFirstShowCallback(L: PLua_State): integer; cdecl;
var lc: TLuaCaller;
  f: integer;
  routine: string;
  form: TCustomForm;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    lc:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      form.AddHandlerFirstShow(lc.NotifyEvent);
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      form.AddHandlerFirstShow(lc.NotifyEvent);
    end;

    luaclass_newClass(L, lc);
    result:=1;
  end;
end;

function customform_unregisterCloseCallback(L: PLua_State): integer; cdecl;
var
  lc: TLuacaller;
  form: TCustomForm;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    lc:=lua_ToCEUserData(L, -1);
    if lc<>nil then
      form.RemoveHandlerClose(lc.CloseEvent);

    lc.Free;
  end;
end;

function customform_registerCloseCallback(L: PLua_State): integer; cdecl;
var lc: TLuaCaller;
  f: integer;
  routine: string;
  form: TCustomForm;
begin
  result:=0;
  form:=luaclass_getClassObject(L);

  if lua_gettop(L)=1 then
  begin
    lc:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      form.AddHandlerClose(lc.CloseEvent);
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      form.AddHandlerClose(lc.CloseEvent);
    end;

    luaclass_newClass(L, lc);
    result:=1;
  end;
end;


procedure customform_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'centerScreen', customform_centerScreen);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnClose', customform_setOnClose);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnClose', customform_getOnClose);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'show', customform_show);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'hide', customform_hide);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'close', customform_close);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'bringToFront', customform_bringToFront);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'showModal', customform_showModal);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isForegroundWindow', customform_isForegroundWindow);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMenu', customform_getMenu);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMenu', customform_setMenu);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBorderStyle', customform_getBorderStyle);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setBorderStyle', customform_setBorderStyle);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'printToRasterImage', customform_printToRasterImage);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'dragNow', customform_dragNow);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'registerCreateCallback', customform_registerCreateCallback);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'registerFirstShowCallback', customform_registerFirstShowCallback);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'registerCloseCallback', customform_registerCloseCallback);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'unregisterCreateCallback', customform_unregisterCreateCallback);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'unregisterFirstShowCallback', customform_unregisterFirstShowCallback);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'unregisterCloseCallback', customform_unregisterCloseCallback);



  luaclass_addPropertyToTable(L, metatable, userdata, 'OnClose', customform_getOnClose, customform_setOnClose);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Menu', customform_getMenu, customform_setMenu);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ModalResult', customform_getModalResult, customform_setModalResult);
  luaclass_addPropertyToTable(L, metatable, userdata, 'FormState', customform_getFormState, nil);
end;

procedure ceform_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customform_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', ceform_saveToFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setDoNotSaveInTable', ceform_setDoNotSaveInTable);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getDoNotSaveInTable', ceform_getDoNotSaveInTable);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveCurrentStateAsDesign', ceform_saveCurrentStateasDesign);

  luaclass_addPropertyToTable(L, metatable, userdata, 'DoNotSaveInTable', ceform_getDoNotSaveInTable, ceform_setDoNotSaveInTable);

end;

procedure initializeLuaForm;
begin
  lua_register(LuaVM, 'createForm', createForm);
  lua_register(LuaVM, 'createFormFromFile', createFormFromFile);

  lua_register(LuaVM, 'form_centerScreen', customform_centerScreen);
  lua_register(LuaVM, 'form_onClose', customform_setOnClose);
  lua_register(LuaVM, 'form_show', customform_show);
  lua_register(LuaVM, 'form_hide', customform_hide);
  lua_register(LuaVM, 'form_close', customform_close);
  lua_register(LuaVM, 'form_showModal', customform_showModal);
  lua_register(LuaVM, 'form_isForegroundWindow', customform_isForegroundWindow);
  lua_register(LuaVM, 'form_getMenu', customform_getMenu);
  lua_register(LuaVM, 'form_setMenu', customform_setMenu);
  lua_register(LuaVM, 'form_saveToFile', ceform_saveToFile);
  lua_register(LuaVM, 'form_setDoNotSaveInTable', ceform_setDoNotSaveInTable);
  lua_register(LuaVM, 'form_getDoNotSaveInTable', ceform_getDoNotSaveInTable);
  lua_register(LuaVM, 'form_printToRasterImage', customform_printToRasterImage);
  lua_register(LuaVM, 'form_dragNow', customform_dragNow);
end;

initialization
  luaclass_register(TCustomForm, customform_addMetaData);
  luaclass_register(TCEForm, ceform_addMetaData);

end.

