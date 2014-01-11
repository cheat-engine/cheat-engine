unit LuaCalendar;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler,
  pluginexports, controls, ComCtrls;

procedure initializeLuaCalendar;

implementation

uses luaclass, LuaWinControl, Calendar;


function createCalendar(L: Plua_State): integer; cdecl;
var
  Calendar: TCalendar;
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

  Calendar:=TCalendar.Create(owner);
  if owner<>nil then
    Calendar.Parent:=owner;

  luaclass_newClass(L, Calendar);
  result:=1;
end;

function calendar_getDateLocalFormat(L: PLua_State): integer; cdecl;
var
  cal: TCalendar;
begin
  cal:=luaclass_getClassObject(L);
  lua_pushstring(L, cal.Date);
  result:=1;
end;

function calendar_getDate(L: PLua_State): integer; cdecl;
var
  cal: TCalendar;
  s: string;
begin
  cal:=luaclass_getClassObject(L);
  DateTimeToString(s,'yyyy-mm-dd',cal.DateTime);
  lua_pushstring(L, s);
  result:=1;
end;

function calendar_setDate(L: PLua_State): integer; cdecl;
var
  cal: TCalendar;
  s: string;
begin
  result:=0;
  cal:=luaclass_getClassObject(L);
  if lua_gettop(l)>=1 then
  begin
    s:=Lua_ToString(L, -1);
    cal.DateTime:=StrToDate(s,'yyyy-mm-dd','-');
  end;
end;

procedure Calendar_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getDateLocalFormat', calendar_getDateLocalFormat);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Date', calendar_getDate, calendar_setDate);
end;

procedure initializeLuaCalendar;
begin
  lua_register(LuaVM, 'createCalendar', createCalendar);
end;

initialization
  luaclass_register(TCustomCalendar, Calendar_addMetaData);

end.
