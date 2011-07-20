unit LuaCanvas;
{
This unit will be used to register TCanvas class methods to lua
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler, fpcanvas;

procedure initializeLuaCanvas;

implementation

function canvas_getPen(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    canvas:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, canvas.Pen);
    result:=1;

  end else lua_pop(L, parameters);
end;

function canvas_getBrush(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    canvas:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, canvas.Brush);
    result:=1;

  end else lua_pop(L, parameters);
end;

function canvas_getFont(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    canvas:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, canvas.Font);
    result:=1;

  end else lua_pop(L, parameters);
end;

function canvas_getWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    canvas:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, canvas.Width);
    result:=1;

  end else lua_pop(L, parameters);
end;

function canvas_getHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    canvas:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, canvas.Height);
    result:=1;

  end else lua_pop(L, parameters);
end;

function canvas_line(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  sourcex: integer;
  sourcey: integer;
  destinationx: integer;
  destinationy: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=5 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    sourcex:=lua_tointeger(L,-parameters+1);
    sourcey:=lua_tointeger(L,-parameters+2);
    destinationx:=lua_tointeger(L,-parameters+3);
    destinationy:=lua_tointeger(L,-parameters+4);

    lua_pop(L, parameters);

    canvas.Line(sourcex, sourcey, destinationx, destinationy);


  end else lua_pop(L, parameters);
end;

function canvas_lineTo(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  destinationx: integer;
  destinationy: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    destinationx:=lua_tointeger(L,-parameters+1);
    destinationy:=lua_tointeger(L,-parameters+2);

    lua_pop(L, parameters);

    canvas.LineTo(destinationx, destinationy);
  end else lua_pop(L, parameters);
end;

function canvas_rect(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=5 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x1:=lua_tointeger(L,-parameters+1);
    y1:=lua_tointeger(L,-parameters+2);
    x2:=lua_tointeger(L,-parameters+3);
    y2:=lua_tointeger(L,-parameters+4);

    lua_pop(L, parameters);

    canvas.Rectangle(x1,y1,x2,y2);
  end else lua_pop(L, parameters);
end;

function canvas_fillRect(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=5 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x1:=lua_tointeger(L,-parameters+1);
    y1:=lua_tointeger(L,-parameters+2);
    x2:=lua_tointeger(L,-parameters+3);
    y2:=lua_tointeger(L,-parameters+4);

    lua_pop(L, parameters);

    canvas.FillRect(x1,y1,x2,y2);
  end else lua_pop(L, parameters);
end;

function canvas_textOut(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x: integer;
  y: integer;
  text: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=4 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x:=lua_tointeger(L,-parameters+1);
    y:=lua_tointeger(L,-parameters+2);
    text:=lua_tostring(L, -parameters+3);

    lua_pop(L, parameters);

    canvas.TextOut(x,y,text);
  end else lua_pop(L, parameters);
end;

function canvas_getTextWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  text: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    text:=lua_tostring(L, -parameters+1);

    lua_pop(L, parameters);

    result:=1;
    lua_pushinteger(L, canvas.GetTextWidth(text));
  end else lua_pop(L, parameters);
end;

function canvas_getTextHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  text: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    text:=lua_tostring(L, -parameters+1);

    lua_pop(L, parameters);

    result:=1;
    lua_pushinteger(L, canvas.GetTextHeight(text));
  end else lua_pop(L, parameters);
end;

function canvas_getPixel(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x:=lua_tointeger(L, -parameters+1);
    y:=lua_tointeger(L, -parameters+2);

    lua_pop(L, parameters);

    result:=1;
    lua_pushinteger(L, canvas.Pixels[x,y]);
  end else lua_pop(L, parameters);
end;

function canvas_setPixel(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x,y: integer;
  color: TColor;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=4 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x:=lua_tointeger(L, -parameters+1);
    y:=lua_tointeger(L, -parameters+2);
    color:=TColor(lua_tointeger(L, -parameters+3));
    lua_pop(L, parameters);

    canvas.Pixels[x,y]:=color;
  end else lua_pop(L, parameters);
end;

function canvas_floodFill(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x,y: integer;
  fill: integer;
  color: TColor;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=3 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x:=lua_tointeger(L, -parameters+1);
    y:=lua_tointeger(L, -parameters+2);

    lua_pop(L, parameters);

    TFPCustomCanvas(canvas).floodfill(x,y);
//    canvas.FloodFill(x,y, canvas.Brush.Color, fsSurface);
    //canvas.FloodFill(x,y);//, color, fill);


  end else lua_pop(L, parameters);
end;

function canvas_ellipse(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=5 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x1:=lua_tointeger(L,-parameters+1);
    y1:=lua_tointeger(L,-parameters+2);
    x2:=lua_tointeger(L,-parameters+3);
    y2:=lua_tointeger(L,-parameters+4);

    lua_pop(L, parameters);

    canvas.Ellipse(x1,y1,x2,y2);
  end else lua_pop(L, parameters);
end;

function canvas_gradientFill(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
  startcolor, stopcolor: tcolor;
  direction: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=8 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x1:=lua_tointeger(L,-parameters+1);
    y1:=lua_tointeger(L,-parameters+2);
    x2:=lua_tointeger(L,-parameters+3);
    y2:=lua_tointeger(L,-parameters+4);

    startcolor:=lua_tointeger(L,-parameters+5);
    stopcolor:=lua_tointeger(L,-parameters+6);
    direction:=lua_tointeger(L,-parameters+7);

    lua_pop(L, parameters);

    canvas.GradientFill(rect(x1,y1,x2,y2), startcolor, stopcolor, TGradientDirection(direction));
  end else lua_pop(L, parameters);
end;

function canvas_copyRect(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  s_canvas: TCanvas;
  d_canvas: TCanvas;
  d_x1,d_y1: integer;
  d_x2,d_y2: integer;
  s_x1,s_y1: integer;
  s_x2,s_y2: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=10 then
  begin
    d_canvas:=lua_touserdata(L,-parameters);
    d_x1:=lua_tointeger(L,-parameters+1);
    d_y1:=lua_tointeger(L,-parameters+2);
    d_x2:=lua_tointeger(L,-parameters+3);
    d_y2:=lua_tointeger(L,-parameters+4);

    s_canvas:=lua_touserdata(L,-parameters+5);
    s_x1:=lua_tointeger(L,-parameters+6);
    s_y1:=lua_tointeger(L,-parameters+7);
    s_x2:=lua_tointeger(L,-parameters+8);
    s_y2:=lua_tointeger(L,-parameters+9);

    lua_pop(L, parameters);

    d_canvas.CopyRect(rect(d_x1, d_y1, d_x2,d_y2), s_canvas, rect(s_x1, s_y1, s_x2,s_y2));
  end else lua_pop(L, parameters);
end;

function canvas_draw(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;
  x,y: integer;
  graphic: TGraphic;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=8 then
  begin
    canvas:=lua_touserdata(L,-parameters);
    x:=lua_tointeger(L,-parameters+1);
    y:=lua_tointeger(L,-parameters+2);
    graphic:=lua_touserdata(L,-parameters+3);


    lua_pop(L, parameters);

    canvas.draw(x,y, graphic);
  end else lua_pop(L, parameters);
end;

function canvas_getPenPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    canvas:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, canvas.PenPos.x);
    lua_pushinteger(L, canvas.PenPos.y);
    result:=2;

  end else lua_pop(L, parameters);
end;

function canvas_setPenPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  canvas: TCanvas;

  pos: tpoint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    canvas:=lua_touserdata(L,-parameters);

    Pos.x:=lua_toInteger(L,-parameters+1);
    Pos.y:=lua_toInteger(L,-parameters+2);
    canvas.PenPos:=pos;
    lua_pop(L, parameters);

  end else lua_pop(L, parameters);
end;

procedure initializeLuaCanvas;
begin
  lua_register(LuaVM, 'canvas_getBrush', canvas_getBrush);
  lua_register(LuaVM, 'canvas_getPen', canvas_getPen);
  lua_register(LuaVM, 'canvas_getFont', canvas_getFont);
  lua_register(LuaVM, 'canvas_getWidth', canvas_getWidth);
  lua_register(LuaVM, 'canvas_getHeight', canvas_getHeight);
  lua_register(LuaVM, 'canvas_line', canvas_line);
  lua_register(LuaVM, 'canvas_lineTo', canvas_lineTo);
  lua_register(LuaVM, 'canvas_rect', canvas_rect);
  lua_register(LuaVM, 'canvas_fillRect', canvas_fillRect);
  lua_register(LuaVM, 'canvas_textOut', canvas_textOut);
  lua_register(LuaVM, 'canvas_getTextWidth', canvas_getTextWidth);
  lua_register(LuaVM, 'canvas_getTextHeight', canvas_getTextHeight);
  lua_register(LuaVM, 'canvas_getPixel', canvas_getPixel);
  lua_register(LuaVM, 'canvas_setPixel', canvas_setPixel);
  lua_register(LuaVM, 'canvas_floodFill', canvas_floodFill);
  lua_register(LuaVM, 'canvas_ellipse', canvas_ellipse);
  lua_register(LuaVM, 'canvas_gradientFill', canvas_gradientFill);
  lua_register(LuaVM, 'canvas_copyRect', canvas_copyRect);
  lua_register(LuaVM, 'canvas_draw', canvas_draw);
  lua_register(LuaVM, 'canvas_getPenPosition', canvas_getPenPosition);
  lua_register(LuaVM, 'canvas_setPenPosition', canvas_setPenPosition);


end;


end.

