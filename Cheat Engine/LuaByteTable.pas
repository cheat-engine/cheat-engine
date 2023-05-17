unit LuaByteTable;

//ByteTable related functions

//A byte table is just a normal table with element ranging from 0 to 255

{$mode delphi}

interface

uses
  Classes, SysUtils, lua;

procedure initializeLuaByteTable;
procedure readBytesFromTable(L: PLua_State; tableindex: integer; p: PByteArray; maxsize: integer);
procedure CreateByteTableFromPointer(L: PLua_state; p: pbytearray; size: integer );

implementation

uses luahandler, frmFloatingPointPanelUnit{$ifdef darwin},mactypes{$endif};

procedure CreateByteTableFromPointer(L: PLua_state; p: pbytearray; size: integer );
var t,i: integer;
begin
  lua_createtable(L, size, 0);

  //lua_newtable(L);
  t:=lua_gettop(L);
  for i:=1 to size do
  begin
//    lua_pushinteger(L, i);
    lua_pushinteger(L, p[i-1]);
    //lua_settable(L, t);
    lua_rawseti(L, t, i);
  end;
end;

procedure readBytesFromTable(L: PLua_State; tableindex: integer; p: PByteArray; maxsize: integer);
var i,j,x: integer;
begin
  for i:=1 to maxsize do
  begin
    lua_pushinteger(L, i);
    lua_gettable(L, tableindex);

    if lua_isnil(L,-1) then
    begin
      lua_pop(L,1);
      for j:=i-1 to maxsize-1 do //zero out the rest
        p[j]:=0;
      exit;
    end;
     p[i-1]:=lua_tointeger(L, -1);
    lua_pop(L,1);
  end;
end;

procedure readBytesFromTableOffset(L: PLua_State; tableindex: integer; p: PByteArray; maxsize: integer; startoffset: integer);
var i,j,x: integer;
begin
  for i:=1 to maxsize do
  begin
    lua_pushinteger(L, i+startoffset);
    lua_gettable(L, tableindex);

    if lua_isnil(L,-1) then
    begin
      lua_pop(L,1);
      for j:=i-1 to maxsize-1 do //zero out the rest
        p[j]:=0;
      exit;
    end;

    p[i-1]:=lua_tointeger(L, -1);
    lua_pop(L,1);
  end;
end;

function wordToByteTable(L: PLua_state): integer; cdecl;
var v: word;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    v:=lua_tointeger(L, 1);
    CreateByteTableFromPointer(L, @v, sizeof(v));
    result:=1;
  end;
end;

function dwordToByteTable(L: PLua_state): integer; cdecl;
var v: dword;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    v:=lua_tointeger(L, 1);
    CreateByteTableFromPointer(L, @v, sizeof(v));
    result:=1;
  end;
end;

function qwordToByteTable(L: PLua_state): integer; cdecl;
var v: qword;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    v:=lua_tointeger(L, 1);
    CreateByteTableFromPointer(L, @v, sizeof(v));
    result:=1;
  end;
end;

function floatToByteTable(L: PLua_state): integer; cdecl;
var v: single;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    v:=lua_tonumber(L, 1);
    CreateByteTableFromPointer(L, @v, sizeof(v));
    result:=1;
  end;
end;

function doubleToByteTable(L: PLua_state): integer; cdecl;
var v: double;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    v:=lua_tonumber(L, 1);
    CreateByteTableFromPointer(L, @v, sizeof(v));
    result:=1;
  end;
end;

function extendedToByteTable(L: PLua_state): integer; cdecl;
var
  v: double;
  e: Extended;
  ex: array [0..9] of byte;

begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    v:=lua_tonumber(L, 1);
{$ifdef cpux86_64}
    doubletoextended(@v,@ex[0]);
    CreateByteTableFromPointer(L, @ex[0], 10);
{$else}
    e:=v;
    CreateByteTableFromPointer(L, @e, sizeof(e));
{$endif}

    result:=1;
  end;
end;

function stringToByteTable(L: PLua_state): integer; cdecl;
var s: pchar;
  len: size_t;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    s:=lua_tolstring(L, 1, @len);

    CreateByteTableFromPointer(L, pbytearray(s), len);
    result:=1;
  end;
end;

function widestringToByteTable(L: PLua_state): integer; cdecl;
var s: string;
  ws: widestring;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    s:=Lua_ToString(L, 1);
    ws:=s;
    CreateByteTableFromPointer(L, @ws[1], length(ws)*2);
    result:=1;
  end;
end;

function byteTableToWord(L: PLua_state): integer; cdecl;
var
  v: word;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=3) then
    begin
      startoffset := lua_tointegerx(L, 3, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
    readBytesFromTableOffset(L, 1, @v, sizeof(v),startoffset);
    if (lua_gettop(L)>=2) and lua_toboolean(L,2) then
      lua_pushinteger(L,smallint(v))
    else
      lua_pushinteger(L,v);
    result:=1;
  end;
end;

function byteTableToDWord(L: PLua_state): integer; cdecl;
var v: dword;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=3) then
    begin
      startoffset := lua_tointegerx(L, 3, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
    readBytesFromTableOffset(L, 1, @v, sizeof(v),startoffset);

    if (lua_gettop(L)>=2) and lua_toboolean(L,2) then
      lua_pushinteger(L,integer(v))
    else
      lua_pushinteger(L,v);
    result:=1;
  end;
end;

function byteTableToQWord(L: PLua_state): integer; cdecl;
var v: qword;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=2) then
    begin
      startoffset := lua_tointegerx(L, 2, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
    readBytesFromTableOffset(L, 1, @v, sizeof(v),startoffset); ;
    lua_pushinteger(L,v);
    result:=1;
  end;
end;

function byteTableToFloat(L: PLua_state): integer; cdecl;
var v: single;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=2) then
    begin
      startoffset := lua_tointegerx(L, 2, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
    readBytesFromTableOffset(L, 1, @v, sizeof(v), startoffset);
    lua_pushnumber(L,v);
    result:=1;
  end;
end;

function byteTableToDouble(L: PLua_state): integer; cdecl;
var v: Double;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=2) then
    begin
      startoffset := lua_tointegerx(L, 2, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
    readBytesFromTableOffset(L, 1, @v, sizeof(v), startoffset);
    lua_pushnumber(L,v);
    result:=1;
  end;
end;


function byteTableToExtended(L: PLua_state): integer; cdecl;
var
  ex: array [0..9] of byte;
  v: double;
  e: extended;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=2) then
    begin
      startoffset := lua_tointegerx(L, 2, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
{$ifdef cpux86_64}
    readBytesFromTableOffset(L, 1, @ex[0], 10, startoffset);
    extendedtodouble(@ex[0],v);
{$else}
    readBytesFromTableOffset(L, 1, @e, sizeof(e), startoffset);
    v:=e;
{$endif}
    lua_pushnumber(L,v);
    result:=1;
  end;
end;

function byteTableToString(L: PLua_state): integer; cdecl;
var s: pchar;
  len: integer;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=2) then
    begin
      startoffset := lua_tointegerx(L, 2, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;
    len:=lua_objlen(L, 1);
    if startoffset > 0 then
      len := len - startoffset;

    getmem(s, len);

    readBytesFromTableOffset(L, 1, @s[0], len, startoffset);
    lua_pushlstring(L, s,len);
    result:=1;
  end;
end;

function byteTableToWideString(L: PLua_state): integer; cdecl;
var s: pwidechar;
  s2: pchar;

  ansis: string;
  len: integer;
  startoffset: integer;
  startoffsetworked: boolean;
begin
  result:=0;
  startoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if (lua_gettop(L)>=2) then
    begin
      startoffset := lua_tointegerx(L, 2, @startoffsetworked);
      if not startoffsetworked then
        startoffset := 0;
    end;

    len:=lua_objlen(L, 1);
    if startoffset > 0 then
      len := len - startoffset;
    getmem(s, len+2);

    s2:=pointer(s);

    readBytesFromTableOffset(L, 1, @s[0], len, startoffset);
    s2[len]:=#0;
    s2[len+1]:=#0;

    ansis:=s;
    lua_pushstring(L, pchar(ansis));
    result:=1;
  end;
end;

procedure initializeLuaByteTable;
begin

  lua_register(LuaVM, 'wordToByteTable', wordToByteTable);
  lua_register(LuaVM, 'dwordToByteTable', dwordToByteTable);
  lua_register(LuaVM, 'qwordToByteTable', qwordToByteTable);

  lua_register(LuaVM, 'floatToByteTable', floatToByteTable);
  lua_register(LuaVM, 'doubleToByteTable', doubleToByteTable);
  lua_register(LuaVM, 'extendedToByteTable', extendedToByteTable);
  lua_register(LuaVM, 'stringToByteTable', stringToByteTable);
  lua_register(LuaVM, 'wideStringToByteTable', wideStringToByteTable);

  lua_register(LuaVM, 'byteTableToWord', byteTableToWord);
  lua_register(LuaVM, 'byteTableToDword', byteTableToDword);
  lua_register(LuaVM, 'byteTableToQword', byteTableToQword);
  lua_register(LuaVM, 'byteTableToFloat', byteTableToFloat);
  lua_register(LuaVM, 'byteTableToDouble', byteTableToDouble);
  lua_register(LuaVM, 'byteTableToExtended', byteTableToExtended);
  lua_register(LuaVM, 'byteTableToString', byteTableToString);
  lua_register(LuaVM, 'byteTableToWideString', byteTableToWideString);


end;

end.

