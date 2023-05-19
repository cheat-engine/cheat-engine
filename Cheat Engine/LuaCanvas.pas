unit LuaCanvas;
{
This unit will be used to register TCanvas class methods to lua
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, GraphType, lua, lualib, lauxlib, LuaHandler, fpcanvas, LCLType, LCLIntf;

procedure initializeLuaCanvas(L: Plua_State);
procedure drawWithMask(DestCanvas:TCanvas; Dx,Dy,Dw,Dh:integer; graph:TRasterImage; Sx,Sy,Sw,Sh:integer);

implementation

uses luaclass, luaobject, textrender;



function canvas_clear(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  canvas.clear;
  result:=0;
end;

function canvas_getPen(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  luaclass_newClass(L, canvas.pen);
  result:=1;
end;

function canvas_getBrush(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  luaclass_newClass(L, canvas.brush);
  result:=1;
end;

function canvas_getFont(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  luaclass_newClass(L, canvas.font);
  result:=1;
end;

function canvas_getWidth(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  lua_pushinteger(L, canvas.width);
  result:=1;
end;

function canvas_getHeight(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  lua_pushinteger(L, canvas.height);
  result:=1;
end;

function canvas_getHandle(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  lua_pushinteger(L, canvas.handle);
  result:=1;
end;

function canvas_setHandle(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  if lua_gettop(L)>=1 then
  begin
    canvas:=luaclass_getClassObject(L);
    canvas.handle:=lua_tointeger(L,1);
  end;
  result:=0;
end;

function canvas_line(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  sourcex: integer;
  sourcey: integer;
  destinationx: integer;
  destinationy: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);

  if lua_gettop(L)>=4 then
  begin
    sourcex:=lua_tointeger(L,-4);
    sourcey:=lua_tointeger(L,-3);
    destinationx:=lua_tointeger(L,-2);
    destinationy:=lua_tointeger(L,-1);
    canvas.Line(sourcex, sourcey, destinationx, destinationy);
  end;
end;

function canvas_lineTo(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  destinationx: integer;
  destinationy: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
  begin
    destinationx:=lua_tointeger(L,-2);
    destinationy:=lua_tointeger(L,-1);
    canvas.LineTo(destinationx, destinationy);
  end;
end;

function canvas_moveTo(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  destinationx: integer;
  destinationy: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
  begin
    destinationx:=lua_tointeger(L,-2);
    destinationy:=lua_tointeger(L,-1);
    canvas.MoveTo(destinationx, destinationy);
  end;
end;

function canvas_rect(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);


  if lua_gettop(L)>=4 then
  begin
    x1:=lua_tointeger(L,-4);
    y1:=lua_tointeger(L,-3);
    x2:=lua_tointeger(L,-2);
    y2:=lua_tointeger(L,-1);

    canvas.Rectangle(x1,y1,x2,y2);
  end;
end;

function canvas_fillRect(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
  r: trect;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    //could be a rect
    if lua_istable(L,1) then
    begin
      r:=lua_toRect(L,1);
      canvas.FillRect(r);
    end;
  end
  else
  if lua_gettop(L)>=4 then
  begin
    x1:=lua_tointeger(L,1);
    y1:=lua_tointeger(L,2);
    x2:=lua_tointeger(L,3);
    y2:=lua_tointeger(L,4);

    canvas.FillRect(x1,y1,x2,y2);
  end;
end;

function canvas_roundRect(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
  rx,ry: integer;
  r: trect;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);

  if lua_gettop(L)=3 then
  begin
    if lua_istable(L,1) then
    begin
      r:=lua_toRect(L,1);
      rx:=lua_tointeger(L,2);
      ry:=lua_tointeger(L,3);

      canvas.RoundRect(r,rx,ry);
    end;
  end
  else
  if lua_gettop(L)>=6 then
  begin
    x1:=lua_tointeger(L,1);
    y1:=lua_tointeger(L,2);
    x2:=lua_tointeger(L,3);
    y2:=lua_tointeger(L,4);

    rx:=lua_tointeger(L,5);
    ry:=lua_tointeger(L,6);

    canvas.RoundRect(x1,y1,x2,y2,rx,ry);
  end;
end;

function canvas_drawFocusRect(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
  r: trect;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=4 then
  begin
    x1:=lua_tointeger(L,1);
    y1:=lua_tointeger(L,2);
    x2:=lua_tointeger(L,3);
    y2:=lua_tointeger(L,4);

    canvas.DrawFocusRect(rect(x1,y1,x2,y2));
  end
  else
  if lua_gettop(L)=1 then
  begin
    r:=lua_toRect(L,1);
    canvas.DrawFocusRect(r);
  end;
end;



function canvas_textOut(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x: integer;
  y: integer;
  text: string;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=3 then
  begin
    x:=lua_tointeger(L, -3);
    y:=lua_tointeger(L, -2);
    text:=lua_tostring(L, -1);
    canvas.TextOut(x,y,text);
  end;
end;

function canvas_textRect(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  rect: TRect;
  x,y: integer;
  text: string;

  outrect: Trect;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=4 then
  begin
    rect:=lua_toRect(L,1);
    x:=lua_tointeger(L,2);
    y:=lua_tointeger(L,3);
    text:=Lua_ToString(L,4);
    outrect:=renderFormattedText(canvas,rect,x,y,text);

    lua_pushrect(L,outrect);
    result:=1;
  end;

end;

function canvas_getTextWidth(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  text: string;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    text:=lua_tostring(L, -1);
    lua_pushinteger(L, canvas.GetTextWidth(text));
    result:=1;
  end;
end;

function canvas_getTextHeight(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  text: string;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    text:=lua_tostring(L, -1);
    lua_pushinteger(L, canvas.GetTextHeight(text));
    result:=1;
  end;
end;

function canvas_getPixel(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x,y: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    x:=lua_tointeger(L, -2);
    y:=lua_tointeger(L, -1);

    lua_pushinteger(L, canvas.Pixels[x,y]);
    result:=1;
  end;
end;

function canvas_setPixel(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x,y: integer;
  color: TColor;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=3 then
  begin
    x:=lua_tointeger(L, -3);
    y:=lua_tointeger(L, -2);
    color:=TColor(lua_tointeger(L, -1));

    canvas.Pixels[x,y]:=color;
  end;
end;

function canvas_floodFill(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x,y: integer;
  fillstyle: TFillStyle;
  fillcolor: TColor;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);

  fillstyle:=fsSurface;

  if lua_gettop(L)>=2 then
  begin
    x:=lua_tointeger(L, 1);
    y:=lua_tointeger(L, 2);

    if lua_gettop(L)>=3 then
      fillcolor:=lua_tointeger(L,3)
    else
      fillcolor:=canvas.Pixels[x,y];

    if lua_gettop(L)>=4 then
      fillstyle:=TFillStyle(lua_tointeger(L,4));

    canvas.floodfill(X, Y,FillColor, FillStyle);
  end;
end;

function canvas_ellipse(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=4 then
  begin
    x1:=lua_tointeger(L,-4);
    y1:=lua_tointeger(L,-3);
    x2:=lua_tointeger(L,-2);
    y2:=lua_tointeger(L,-1);
    canvas.Ellipse(x1,y1,x2,y2);
  end;
end;

function canvas_gradientFill(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x1,y1: integer;
  x2,y2: integer;
  startcolor, stopcolor: tcolor;
  direction: integer;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=7 then
  begin
    x1:=lua_tointeger(L,-7);
    y1:=lua_tointeger(L,-6);
    x2:=lua_tointeger(L,-5);
    y2:=lua_tointeger(L,-4);

    startcolor:=lua_tointeger(L,-3);
    stopcolor:=lua_tointeger(L,-2);
    direction:=lua_tointeger(L,-1);

    canvas.GradientFill(rect(x1,y1,x2,y2), startcolor, stopcolor, TGradientDirection(direction));
  end;
end;

procedure drawWithMask(DestCanvas:TCanvas; Dx,Dy,Dw,Dh:integer; graph:TRasterImage; Sx,Sy,Sw,Sh:integer);
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (graph.Width=0) or (graph.Height=0)
  then Exit;

  if graph.Masked then
    UseMaskHandle:=graph.MaskHandle
  else
    UseMaskHandle:=0;

  SrcDC := graph.Canvas.GetUpdatedHandle([csHandleValid]);
  DestCanvas.Changing;
  DestDC := DestCanvas.GetUpdatedHandle([csHandleValid]);
  StretchMaskBlt(DestDC, Dx,Dy,Dw,Dh,
                 SrcDC , Sx,Sy,Sw,Sh, UseMaskHandle,Sx,Sy,DestCanvas.CopyMode);
  DestCanvas.Changed;
end;

function canvas_drawWithMask(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
  d_canvas: TCanvas;
  d_x1,d_y1: integer;
  d_x2,d_y2: integer;
  s_x1,s_y1: integer;
  s_x2,s_y2: integer;
begin
  result:=0;
  d_canvas:=luaclass_getClassObject(L);

  if lua_gettop(L)>=9 then
  begin
    d_x1:=lua_tointeger(L,-9);
    d_y1:=lua_tointeger(L,-8);
    d_x2:=lua_tointeger(L,-7);
    d_y2:=lua_tointeger(L,-6);

    graphic:=lua_toceuserdata(L,-5);
    s_x1:=lua_tointeger(L,-4);
    s_y1:=lua_tointeger(L,-3);
    s_x2:=lua_tointeger(L,-2);
    s_y2:=lua_tointeger(L,-1);
    drawWithMask(d_canvas, d_x1, d_y1, d_x2-d_x1, d_y2-d_y1, TRasterImage(graphic), s_x1, s_y1, s_x2-s_x1,s_y2-s_y1);
  end;
end;

function canvas_copyRect(L: PLua_State): integer; cdecl;
var
  s_canvas: TCanvas;
  d_canvas: TCanvas;
  d_x1,d_y1: integer;
  d_x2,d_y2: integer;
  s_x1,s_y1: integer;
  s_x2,s_y2: integer;
begin
  result:=0;
  d_canvas:=luaclass_getClassObject(L);

  if lua_gettop(L)>=9 then
  begin
    d_x1:=lua_tointeger(L,-9);
    d_y1:=lua_tointeger(L,-8);
    d_x2:=lua_tointeger(L,-7);
    d_y2:=lua_tointeger(L,-6);

    s_canvas:=lua_toceuserdata(L,-5);
    s_x1:=lua_tointeger(L,-4);
    s_y1:=lua_tointeger(L,-3);
    s_x2:=lua_tointeger(L,-2);
    s_y2:=lua_tointeger(L,-1);

    d_canvas.CopyRect(rect(d_x1, d_y1, d_x2,d_y2), s_canvas, rect(s_x1, s_y1, s_x2,s_y2));
  end;
end;

function canvas_draw(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  x,y: integer;
  graphic: TGraphic;

begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=3 then
  begin
    x:=lua_tointeger(L,1);
    y:=lua_tointeger(L,2);
    graphic:=lua_toceuserdata(L,3);

    canvas.draw(x,y, graphic);
  end;
end;

function canvas_stretchDraw(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  r: trect;
  graphic: TGraphic;

begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    r:=lua_toRect(L,1);
    graphic:=lua_toceuserdata(L,2);

    canvas.StretchDraw(r,graphic);
  end;
end;

function canvas_getPenPosition(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
begin
  canvas:=luaclass_getClassObject(L);
  lua_pushinteger(L, canvas.PenPos.x);
  lua_pushinteger(L, canvas.PenPos.y);
  result:=2;
end;

function canvas_setPenPosition(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;

  pos: tpoint;
begin
  result:=0;
  canvas:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    Pos.x:=lua_toInteger(L,-2);
    Pos.y:=lua_toInteger(L,-1);
    canvas.PenPos:=pos;
  end;
end;

function canvas_getClipRect(L: PLua_State): integer; cdecl;
var
  canvas: TCanvas;
  cr: Trect;
begin
  canvas:=luaclass_getClassObject(L);
  cr:=canvas.ClipRect;
  lua_pushrect(L, cr);
  result:=1;
end;


procedure canvas_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', canvas_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBrush', canvas_getBrush);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPen', canvas_getPen);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFont', canvas_getFont);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWidth', canvas_getWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getHeight', canvas_getHeight);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'line', canvas_line);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'lineTo', canvas_lineTo);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'moveTo', canvas_moveTo);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'rect', canvas_rect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fillRect', canvas_fillRect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'roundRect', canvas_roundRect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'textOut', canvas_textOut);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'textRect', canvas_textRect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTextWidth', canvas_getTextWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTextHeight', canvas_getTextHeight);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPixel', canvas_getPixel);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPixel', canvas_setPixel);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'floodFill', canvas_floodFill);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'ellipse', canvas_ellipse);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'gradientFill', canvas_gradientFill);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'copyRect', canvas_copyRect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'draw', canvas_draw);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'stretchDraw', canvas_stretchDraw);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'drawWithMask', canvas_drawWithMask);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPenPosition', canvas_getPenPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPenPosition', canvas_setPenPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getClipRect', canvas_getClipRect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'drawFocusRect', canvas_drawFocusRect);


  Luaclass_addPropertyToTable(L, metatable, userdata, 'Brush', canvas_getBrush, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Pen', canvas_getPen, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Font', canvas_getFont, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Width', canvas_getWidth, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Height', canvas_getHeight, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Handle', canvas_getHandle, canvas_setHandle);

end;

procedure initializeLuaCanvas;
begin
  lua_register(L, 'canvas_getBrush', canvas_getBrush);
  lua_register(L, 'canvas_getPen', canvas_getPen);
  lua_register(L, 'canvas_getFont', canvas_getFont);
  lua_register(L, 'canvas_getWidth', canvas_getWidth);
  lua_register(L, 'canvas_getHeight', canvas_getHeight);
  lua_register(L, 'canvas_line', canvas_line);
  lua_register(L, 'canvas_lineTo', canvas_lineTo);
  lua_register(L, 'canvas_rect', canvas_rect);
  lua_register(L, 'canvas_fillRect', canvas_fillRect);
  lua_register(L, 'canvas_textOut', canvas_textOut);
  lua_register(L, 'canvas_getTextWidth', canvas_getTextWidth);
  lua_register(L, 'canvas_getTextHeight', canvas_getTextHeight);
  lua_register(L, 'canvas_getPixel', canvas_getPixel);
  lua_register(L, 'canvas_setPixel', canvas_setPixel);
  lua_register(L, 'canvas_floodFill', canvas_floodFill);
  lua_register(L, 'canvas_ellipse', canvas_ellipse);
  lua_register(L, 'canvas_gradientFill', canvas_gradientFill);
  lua_register(L, 'canvas_copyRect', canvas_copyRect);
  lua_register(L, 'canvas_draw', canvas_draw);
  lua_register(L, 'canvas_getPenPosition', canvas_getPenPosition);
  lua_register(L, 'canvas_setPenPosition', canvas_setPenPosition);
end;

initialization
  luaclass_register(TCanvas, canvas_addMetaData);


end.

