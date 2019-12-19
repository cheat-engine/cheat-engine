unit gamepanel;


{$mode objfpc}{$H+}

interface

uses
  windows, lmessages, Classes, SysUtils, controls, ExtCtrls, GL, GLext, glu,
  graphics, renderobject, dialogs, types;

//optimizations will come later, or never. It's just a tutorial/example for CE. Not a high speed competitive first person shooter...
type
  TMEvent = function(TGamePanel: TObject; meventtype: integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean of Object;
  TKEvent = function(TGamePanel: TObject; keventtype: integer; Key: Word; Shift: TShiftState):boolean of Object;


  TGamePanel=class(Tcustompanel)
  private
    glrc: HGLRC;
    ticker: TTimer;
    oldWndProc: TWndMethod;
    fOnGameTick: TNotifyEvent;
    fOnRender: TNotifyEvent;

    keyEventHandlers: array of TKEvent;
    mouseEventHandlers: array of TMEvent;
    procedure mywndproc(var TheMessage: TLMessage);
    procedure tick(sender: tobject);
  public
    background:record
      r,g,b: single;
    end;
    constructor Create(TheOwner: TComponent); override;
    procedure AddKeyEventHandler(keyevent: TKEvent; position: integer=-1);
    procedure RemoveKeyEventHandler(keyevent: TKEvent);
    procedure AddMouseEventHandler(mouseEvent: TMEvent; position: integer=-1);
    procedure RemoveMouseEventHandler(mouseEvent: TMEvent);
    procedure render;
    function PixelPosToGamePos(x,y: integer): TPointf;
    function GamePosToPixelPos(x,y: single): TPoint;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetParent(NewParent: TWinControl); override;
  published
    property OnGameTick: TNotifyEvent read fOnGameTick write fOnGameTick;
    property OnGameRender: TNotifyEvent read fOnRender write fOnRender;

  end;

implementation


var
  z: boolean=false;
  t: array [0..10] of gluint;
  img: tpicture;

  pixels: array [0..11] of single=(0,1,0,0.5,0.5,0.5,1,1,1,1,0,0);

  pp: pointer;


  r: single=0;

procedure TGamePanel.KeyDown(var Key: Word; Shift: TShiftState);
var i: integer;
begin

  inherited KeyDown(Key, Shift);
  for i:=0 to length(KeyEventHandlers)-1 do
    if keyEventHandlers[i](self, 0, key, shift) then break;
end;

procedure TGamePanel.KeyUp(var Key: Word; Shift: TShiftState);
var i: integer;
begin
  inherited KeyUp(Key, Shift);
  for i:=0 to length(KeyEventHandlers)-1 do
    if keyEventHandlers[i](self, 1, key, shift) then break;
end;


function TGamePanel.GamePosToPixelPos(x,y: single): TPoint;
begin
  result.x:=trunc((1+x)*(width/2));
  result.y:=trunc((1+y)*(height/2));
end;

function TGamePanel.PixelPosToGamePos(x,y: integer): TPointf;
begin
  result.x:=(x / (width/2))-1;
  result.y:=(y / (height/2))-1;
end;

procedure TGamePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  inherited MouseMove(Shift, x,y);
  for i:=0 to length(mouseEventHandlers)-1 do
    if mouseEventHandlers[i](self, 2, mbLeft, Shift, x,y) then break;
end;

procedure TGamePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  inherited MouseDown(Button, Shift, x,y);
  for i:=0 to length(mouseEventHandlers)-1 do
    if mouseEventHandlers[i](self, 1, Button, Shift, x,y) then break;

end;

procedure TGamePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  inherited MouseDown(Button, Shift, x,y);
  for i:=0 to length(mouseEventHandlers)-1 do
    if mouseEventHandlers[i](self, 0, Button, Shift, x,y) then break;
end;

procedure TGamePanel.AddKeyEventHandler(KeyEvent: TKEvent; position: integer=-1);
var i,j: integer;
begin
  setlength(KeyEventHandlers,length(KeyEventHandlers)+1);

  if position=-1 then //at the end (highest order)
    KeyEventHandlers[length(KeyEventHandlers)-1]:=KeyEvent
  else
  begin
    for i:=position+1 to length(KeyEventHandlers)-2 do
      KeyEventHandlers[i]:=KeyEventHandlers[i+1];

    KeyEventHandlers[position]:=KeyEvent;
  end;
end;

procedure TGamePanel.RemoveKeyEventHandler(KeyEvent: TKEvent);
var i,j: integer;
begin
  i:=0;
  while i<length(KeyEventHandlers) do
  begin
    if (tmethod(KeyEventHandlers[i]).Data=tmethod(KeyEvent).Data) and (tmethod(KeyEventHandlers[i]).Code=tmethod(KeyEvent).Code) then
    begin
      for j:=i to length(KeyEventHandlers)-2 do
        KeyEventhandlers[j]:=KeyEventHandlers[j+1];

      setlength(KeyEventhandlers,length(KeyEventhandlers)-1)
    end
    else
      inc(i);
  end;
end;

procedure TGamePanel.AddMouseEventHandler(mouseEvent: TMEvent; position: integer=-1);
var i,j: integer;
begin
  setlength(mouseEventHandlers,length(mouseEventHandlers)+1);

  if position=-1 then //at the end (highest order)
    mouseEventHandlers[length(mouseEventHandlers)-1]:=mouseEvent
  else
  begin
    for i:=position+1 to length(mouseEventHandlers)-2 do
      mouseEventHandlers[i]:=mouseEventHandlers[i+1];

    mouseEventHandlers[position]:=mouseEvent;
  end;
end;

procedure TGamePanel.RemoveMouseEventHandler(mouseEvent: TMEvent);
var i,j: integer;
begin
  i:=0;
  while i<length(mouseEventHandlers) do
  begin
    if (tmethod(mouseEventHandlers[i]).Data=tmethod(mouseEvent).Data) and (tmethod(mouseEventHandlers[i]).Code=tmethod(mouseEvent).Code) then
    begin
      for j:=i to length(mouseEventHandlers)-2 do
        mouseEventhandlers[j]:=mouseEventHandlers[j+1];

      setlength(mouseEventhandlers,length(mouseEventhandlers)-1)
    end
    else
      inc(i);
  end;
end;


procedure TGamePanel.render;
begin
  //render the 'game'

  wglMakeCurrent(canvas.handle, glrc);

//  glViewport(0, 0, Width, Height);
  glViewport(0, 0, Width,Height); //width, height



//  glMatrixMode(GL_PROJECTION);
//  glLoadIdentity();
//  gluOrtho2D(-1,1,-1,1); //default anyhow

  //setup some states
  glClearColor(background.r, background.g, background.b, 1.0); // Set background color to black and opaque
  glClear(GL_COLOR_BUFFER_BIT);         // Clear the color buffer (background)

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();


  if assigned(glActiveTexture)=false then
    pointer(glActiveTexture):=wglGetProcAddress('glActiveTexture');


  glEnable(GL_TEXTURE_2D);
  glTexEnvf(GL_TEXTURE_2D,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glDepthMask(GL_FALSE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glColor4f(1.0,1.0,1.0,1.0);//Replace this alpha for transparency

  if assigned(fOnRender) then
    fOnRender(self);

  glFlush();  // Render now


  //present
  SwapBuffers(canvas.handle);
end;

procedure TGamePanel.mywndproc(var TheMessage: TLMessage);
begin
  if TheMessage.msg=lm_paint then
    render()
  else
    oldWndProc(TheMessage);
end;

procedure TGamePanel.tick(sender: tobject);
begin
  if assigned(fOnGameTick) then
    fOnGameTick(self);

  render;
end;

procedure TGamePanel.SetParent(NewParent: TWinControl);
var
  pfd: TPixelFormatDescriptor;
  i: integer;
  oldparent: TWinControl;
begin
  oldparent:=parent;
  inherited SetParent(NewParent);

  if (NewParent<>nil) and (oldparent=nil) then
  begin
    glrc:=wglCreateContext(canvas.handle);

    if glrc=0 then
    begin
      pfd.nSize:=sizeof(pfd);
      pfd.nVersion:=1;
      pfd.dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
      pfd.iPixelType:=PFD_TYPE_RGBA;
      pfd.cColorBits:=24;
      pfd.cRedBits:=0;
      pfd.cRedShift:=0;
      pfd.cGreenBits:=0;
      pfd.cGreenShift:=0;
      pfd.cBlueBits:=0;
      pfd.cBlueShift:=0;
      pfd.cAlphaBits:=0;
      pfd.cAlphaShift:=0;
      pfd.cAccumBits:=0;
      pfd.cAccumRedBits:=0;
      pfd.cAccumGreenBits:=0;
      pfd.cAccumBlueBits:=0;
      pfd.cAccumAlphaBits:=0;
      pfd.cDepthBits:=16;
      pfd.cStencilBits:=0;
      pfd.cAuxBuffers:=0;
      pfd.iLayerType:=PFD_MAIN_PLANE;
      pfd.bReserved:=0;
      pfd.dwLayerMask:=0;
      pfd.dwVisibleMask:=0;
      pfd.dwDamageMask:=0;



      i:=ChoosePixelFormat(canvas.handle, @pfd);
      SetPixelFormat(canvas.handle, i, @pfd);

      glrc:=wglCreateContext(canvas.handle);

      wglMakeCurrent(canvas.handle, glrc);

      if Load_GL_version_1_3()=false then
      begin
        ticker.enabled:=false;
        MessageDlg('OpenGL 1.3 or later is required',mtError,[mbok],0);
        ExitProcess(13);
      end;
    end;
  end;

  SetFocus;
end;

constructor TGamePanel.Create(TheOwner: TComponent);
begin
 // if Load_GL_version_1_3=false then raise exception.create('Opengl failed to load');

  inherited create(TheOwner);
  oldWndProc:=WindowProc;

  windowproc:=TWndMethod(@mywndproc);

  ticker:=ttimer.Create(self);
  ticker.OnTimer:=@tick;
  ticker.Interval:=16;
  ticker.enabled:=true;
end;


initialization



end.


