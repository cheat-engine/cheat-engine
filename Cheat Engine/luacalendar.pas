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

function setFormat(s:string): string;
begin
    if (uppercase(s)='LOCALSHORT') or (s='') then
      result:=FormatSettings.ShortDateFormat
    else if (uppercase(s)='LOCALLONG') then
      result:=FormatSettings.LongDateFormat
    else
      result:=s;
end;

function StrToDateGuessSep(s,f :string): TDateTime;
var date: TDateTime;
begin
  try
    date:=StrToDate(s,f,'-');
  except
      try
        date:=StrToDate(s,f,'/');
      except
        date:=StrToDate(s,f,'.');
      end;
  end;
  result:=date;
end;

function calendar_getDateLocalFormat(L: PLua_State): integer; cdecl;
var
  cal: TCalendar;
begin
  cal:=luaclass_getClassObject(L);
  lua_pushstring(L, AnsiToUtf8(cal.Date) );
  result:=1;
end;

function calendar_getDate(L: PLua_State): integer; cdecl;
var
  cal: TCalendar;
  s: string;
begin
  cal:=luaclass_getClassObject(L);
  DateTimeToString(s,'yyyy-mm-dd',cal.DateTime);
  lua_pushstring(L, AnsiToUtf8(s) );
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

function calendarDiffDays(L: PLua_state): integer; cdecl;
var s, format: string;
    parameters: integer;
    number: integer;
begin
  result:=1;
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    if parameters=3 then format:=setFormat( Lua_ToString(L,3) )
    else                 format:='yyyy-mm-dd';

    s:=Lua_ToString(L, 1);
    number:=trunc( StrToDateGuessSep(s,format) );
    s:=Lua_ToString(L, 2);
    number:=number - trunc( StrToDateGuessSep(s,format) );
    lua_pushinteger(L, number);
  end
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function calendarDateToNumber(L: PLua_state): integer; cdecl;
var s, format: string;
    parameters: integer;
begin
  result:=1;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    if parameters=2 then format:=setFormat( Lua_ToString(L,2) )
    else                 format:='yyyy-mm-dd';

    s:=Lua_ToString(L, 1);
    lua_pushinteger(L, trunc( StrToDateGuessSep(s,format)) );
  end
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function calendarNumberToDate(L: PLua_state): integer; cdecl;
var s, format: string;
    parameters: integer;
begin
  result:=1;
  parameters:=lua_gettop(L);

  if parameters>=1 then
  begin
    if parameters=2 then format:=setFormat( Lua_ToString(L,2) )
    else                 format:='yyyy-mm-dd';

    DateTimeToString(s, format, Lua_ToInteger(L, 1));
    lua_pushstring(L, AnsiToUtf8(s) );
  end
  else
    raise exception.create(rsIncorrectNumberOfParameters);
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
  lua_register(LuaVM, 'calendarDiffDays', calendarDiffDays);
  lua_register(LuaVM, 'calendarDateToNumber', calendarDateToNumber);
  lua_register(LuaVM, 'calendarNumberToDate', calendarNumberToDate);
end;

initialization
  luaclass_register(TCustomCalendar, Calendar_addMetaData);

end.
