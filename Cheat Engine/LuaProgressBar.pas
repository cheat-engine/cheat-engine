unit LuaProgressBar;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, ceguicomponents,
  pluginexports, controls, ComCtrls;

procedure initializeLuaProgressBar;

implementation

uses luaclass, LuaWinControl;


function createProgressBar(L: Plua_State): integer; cdecl;
var
  ProgressBar: TProgressBar;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_toceuserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ProgressBar:=TProgressBar.Create(owner);
  if owner<>nil then
    ProgressBar.Parent:=owner;

  luaclass_newClass(L, ProgressBar);
  result:=1;
end;

function progressbar_stepIt(L: Plua_State): integer; cdecl;
var progressbar: TCustomProgressBar;
begin
  result:=0;
  progressbar:=luaclass_getClassObject(L);
  progressbar.StepIt;
end;

function progressbar_stepBy(L: Plua_State): integer; cdecl;
var
  progressbar: TCustomProgressBar;
begin
  result:=0;
  progressbar:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    progressbar.StepBy(lua_tointeger(L, -1));
end;


function progressbar_getMax(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
begin
  progressbar:=luaclass_getClassObject(L);
  lua_pushinteger(L, progressbar.Max);
  result:=1;
end;

function progressbar_setMax(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  progressbar:=luaclass_getClassObject(L);
  if lua_gettop(l)>=1 then
    progressbar.max:=lua_tointeger(L, -1);
end;

function progressbar_getMin(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
begin
  progressbar:=luaclass_getClassObject(L);
  lua_pushinteger(L, progressbar.Min);
  result:=1;
end;

function progressbar_setMin(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  progressbar:=luaclass_getClassObject(L);
  if lua_gettop(l)>=1 then
    progressbar.Min:=lua_tointeger(L, -1);
end;

function progressbar_getPosition(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
begin
  progressbar:=luaclass_getClassObject(L);
  lua_pushinteger(L, progressbar.Position);
  result:=1;
end;

function progressbar_setPosition(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  progressbar:=luaclass_getClassObject(L);
  if lua_gettop(l)>=1 then
    progressbar.Position:=lua_tointeger(L, -1);
end;

function progressbar_setPosition2(L: PLua_State): integer; cdecl;
var
  progressbar: Tcustomprogressbar;
  newPos: integer;
begin
  result:=0;
  progressbar:=luaclass_getClassObject(L);
  if lua_gettop(l)>=1 then
  begin
    newPos:=lua_tointeger(L, -1);
    if newPos>progressbar.Position then
    begin
      if newPos=progressbar.Max then
      begin
        progressbar.Max:=progressbar.Max+1;
        progressbar.Position:=newPos+1;
        progressbar.Position:=newPos;
        progressbar.Max:=progressbar.Max-1;
      end
      else
      begin
        progressbar.Position:=newPos+1;
        progressbar.Position:=newPos;
      end;
    end
    else
      progressbar.Position:=newPos;
  end;
end;

procedure progressbar_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'stepIt', progressbar_stepIt);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'stepBy', progressbar_stepBy);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMax', progressbar_getMax);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMax', progressbar_setMax);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMin', progressbar_getMin);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMin', progressbar_setMin);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPosition', progressbar_getPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPosition', progressbar_setPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPosition2', progressbar_setPosition2); // without slow progress animation on win7 and later

  luaclass_addPropertyToTable(L, metatable, userdata, 'Min', progressbar_getMin, progressbar_setMin);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Max', progressbar_getMax, progressbar_setMax);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Position', progressbar_getPosition, progressbar_setPosition);
end;

procedure initializeLuaProgressBar;
begin
  lua_register(LuaVM, 'createProgressBar', createProgressBar);
  lua_register(LuaVM, 'progressbar_stepIt', progressbar_stepIt);
  lua_register(LuaVM, 'progressbar_stepBy', progressbar_stepBy);
  lua_register(LuaVM, 'progressbar_getMax', progressbar_getMax);
  lua_register(LuaVM, 'progressbar_setMax', progressbar_setMax);
  lua_register(LuaVM, 'progressbar_getMin', progressbar_getMin);
  lua_register(LuaVM, 'progressbar_setMin', progressbar_setMin);
  lua_register(LuaVM, 'progressbar_getPosition', progressbar_getPosition);
  lua_register(LuaVM, 'progressbar_setPosition', progressbar_setPosition);
end;

initialization
  luaclass_register(TCustomProgressBar, progressbar_addMetaData);

end.

