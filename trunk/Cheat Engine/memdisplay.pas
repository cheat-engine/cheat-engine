unit memdisplay;
{
Will render a memory block using an opengl drawPixel command. Display options are userchangable

Should also have the ability to highlight blocks of memory with a specific color

setPointer(block, address)
setWidth(integer)
setFormat(x)

id=createOverlay(address, size)
setOverlayColor(id, color)

render()
  Render the graphics bitmap
  Then render the overlays

setUpdateInterval()


expose: onKeyPress


When the user scrolls left and right the limits will be the  pitch
When the user scrolls up or down, it will send an onData event when the new line on is outside of it's current region
If onData returns false, the scroll will not succeed and be put back to the max allowed


}

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, ExtCtrls, Controls, LMessages, Graphics, GL, glu,
  math, dialogs;


type TCurrentOverlay=record
  x,y,width,height: integer;
  data: pointer;
end;
  PCurrentOverlay=^TCurrentOverlay;


type TOnDataEvent=function(newAddress: ptruint; PreferedMinimumSize: integer; var newbase: pointer; var newsize: integer): boolean of object;
//This event will be called when the displayed size is changed, or when the position is moved, and it won't fit in the current block anymore

type
  TMemDisplay=class(Tcustompanel)
  private
    oldWndProc: TWndMethod;

    hglrc: HGLRC;
    updater: TIdleTimer;

    address: ptruint;
    p: pointer;
    size: integer;

    fZoom: single;
    fXpos, fYpos: integer;
    fPitch: integer;
    fPixelFormat: integer;
    fPixelByteSize: integer; //the number of bytes one pixel exists of (I do not support monochrome...)

    isDragging: boolean;
    DragOrigin: TPoint;
    PosOrigin: TPoint;

    fOnData: TOnDataEvent;

    hasFont: boolean;

    procedure wndproc(var TheMessage: TLMessage);
    procedure Resize; override;
    procedure updaterevent(sender: TObject);
    procedure setupFont;
  protected
    mapr, mapg, mapb: array of single;
    procedure SetParent(NewParent: TWinControl); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    procedure LimitCoordinates;
    procedure RecenterDrag;
  public
    totaldiff: qword;
    ticks: integer;
    lastdiff: qword;
    function MoveTo(xpos, ypos: integer): boolean;
    procedure setFormat(format: integer);
    procedure setPitch(pitch: integer);
    procedure setPointer(address: ptruint; p: pointer; size: integer);
    procedure update; override;
    procedure repaint; override;
    procedure render;

    function GetTopLeftPixelCoordinates: TPoint; //returns the unzoomed coordinates of the selected pixel
    function GetBottomRightPixelCoordinates: TPoint;


    property onData: TOnDataEvent read fOnData write fOnData;
    //property getOffset: integer;

    constructor Create(TheOwner: TComponent); override;
  end;


implementation

procedure TMemDisplay.setPointer(address: ptruint; p: pointer; size: integer);
begin
  self.address:=address;
  self.p:=p;
  self.size:=size;
end;

procedure TMemDisplay.setFormat(format: integer);
begin
  fPixelFormat:=format;


  case fpixelformat of
    GL_RGB: fPixelByteSize:=3;
    GL_RGBA: fPixelByteSize:=4;
    GL_LUMINANCE_ALPHA: fPixelByteSize:=3;
    else
      fPixelByteSize:=1;
  end;
end;

procedure TMemDisplay.setPitch(pitch: integer);
begin
  fPitch:=pitch;

  if fPitch<=0 then
    fPitch:=1;
end;

procedure TMemDisplay.updaterevent(sender: TObject);
begin
  render;
end;

function TMemdisplay.GetTopLeftPixelCoordinates: TPoint;
begin
  result.x:=trunc(-fxpos / fzoom);
  result.y:=trunc(fypos/fzoom);
end;

function TMemdisplay.GetBottomRightPixelCoordinates: TPoint;
begin
  result.x:=min((fPitch div fPixelByteSize)-1 , trunc((-fxpos+width) / fzoom) );  //trunc((-fxpos+width) / fzoom);
  result.y:=min((size div fPitch)-1, trunc((fypos+height)/fzoom));
end;

procedure TMemdisplay.LimitCoordinates;
//Sets the coordinates to their max possible allowed. Preference to top left if both sides are out of range
var
  visiblepixelwidth: integer;
  visiblerows: integer;
  newp: pointer;
  newsize: integer;

  bytesPerRow: integer;
  pixelsPerRow: integer;
  preferedsize: integer;
  a: ptruint;
  f: single;

  row: single;
begin
  visiblepixelwidth:=trunc(Width / fZoom);
  visiblerows:=trunc(height / fzoom);
  bytesperrow:=fPitch;
  pixelsperrow:=bytesperrow div fPixelByteSize;


  if (-fxpos / fzoom) >(pixelsperrow)-(visiblepixelwidth div 2) then
    fxpos:=-trunc(((pixelsperrow)-(visiblepixelwidth div 2)) * fZoom);

  if fXpos>0 then
    fXpos:=0;

  //check if the ypos is outside the allowed region

  row:=fYpos / fZoom;
  if row*bytesperrow>size-((visiblerows*bytesperrow)) then //outside
  begin
    if assigned(fOnData) and fOnData(a,preferedsize,newp,newsize) then
    begin
      address:=a;
      p:=newp;
      size:=newsize;

      //set the new ypos to the new topleft position
      f:=fypos / fZoom;
      f:=abs(f-trunc(f));
      f:=f*fZoom;

      fypos:=trunc(fzoom-f);

      LimitCoordinates;
    end
    else
    begin
      //not overriden so no new data, scroll till the half
      if row*bytesperrow>size-((visiblerows*bytesperrow)/2) then
      begin
        row:=(size div bytesperrow)-(visiblerows/2);

        fypos:=trunc(row*fzoom);

        if isDragging then
          RecenterDrag;

      end;
    end;

  end;

  {

  }


  if fYpos<0 then
  begin
    a:=self.address-trunc((-fypos / fzoom)*bytesperrow);

    preferedsize:=bytesPerRow * height;

    if assigned(fOnData) and fOnData(a,preferedsize,newp,newsize) then
    begin
      address:=a;
      p:=newp;
      size:=newsize;

      //set the new ypos to the new topleft position
      f:=fypos / fZoom;
      f:=abs(f-trunc(f));
      f:=f*fZoom;
      fypos:=trunc(fZoom-f);

      LimitCoordinates //recheck with the new ypos. (in case of size)
    end
    else
    begin
      fYpos:=0;
      if isDragging then
        RecenterDrag;
    end;

  end;


  //if so, ask an external event for new data. If it returns false, set to max allowed

end;

function TMemDisplay.MoveTo(Xpos, Ypos: integer): boolean;
begin
  result:=true;

  fXpos:=Xpos;
  fYpos:=Ypos;

  LimitCoordinates;

  render;
end;

procedure TMemDisplay.RecenterDrag;
var p: tpoint;
begin
  if isDragging then
  begin
    p:=self.ScreenToClient(mouse.cursorpos);

    MouseDown(mbLeft, [], p.x, p.y);
  end;
end;

Procedure TMemDisplay.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button=mbleft then
  begin
    isDragging:=true;
    DragOrigin.x:=x;
    DragOrigin.y:=y;
    PosOrigin.x:=fXpos;
    PosOrigin.y:=fYpos;
  end;

end;

procedure TMemDisplay.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if isDragging then
    MoveTo(PosOrigin.x-(DragOrigin.x-x), PosOrigin.y+(DragOrigin.y-y));
end;

procedure TMemDisplay.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button=mbleft then
    isDragging:=false;

  //todo: Add a pixel click event handler

  render;
end;

function TMemDisplay.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var oldx, oldy, newx,newy: single;
  p: tpoint;
begin

  {//center
  oldx:=(-fXpos+(width / 2))/fZoom;
  oldy:=(fYpos+(height /2))/fZoom;
  }

  //mousepos

  oldx:=(-fXpos+(MousePos.x))/fZoom;
  oldy:=(fYpos+(MousePos.y))/fZoom;



  //oldx,oldy=center



  if WheelDelta>0 then
  begin
    if fzoom<256 then
    begin
      //zoom in
      //get the pixel at center of the screen

      fZoom:=fZoom * 2;
      setupfont;

      fXpos:=trunc(-oldx*fZoom+(MousePos.x));

      newx:=(-fXpos+(MousePos.x))/fZoom;

      fypos:=trunc(oldy*fZoom-(MousePos.y));
      newy:=(fYpos+(MousePos.y))/fZoom;



    end;
  end
  else if WheelDelta<0 then
  begin
    if fZoom>0.1 then
    begin
      //zoom out
      fZoom:=fZoom / 2;
      setupfont;

      fXpos:=trunc(-oldx*fZoom+(MousePos.x));

      newx:=(-fXpos+(MousePos.x))/fZoom;

      fypos:=trunc(oldy*fZoom-(MousePos.y));
      newy:=(fYpos+(MousePos.y))/fZoom;

    end;
  end;


  LimitCoordinates;
  setupFont;
  render;


  result:=true;
end;

procedure TMemDisplay.setupFont;
var z: integer;
begin

  z:=trunc(fZoom/4);
  if z>0 then
  begin
    canvas.font.Height:=z;
    hasFont:=wglUseFontBitmaps(canvas.handle, 0, 255, 1000);
  end
  else
    hasFont:=false;

end;

procedure TMemDisplay.SetParent(NewParent: TWinControl);
var
  pfd: TPixelFormatDescriptor;
  i: integer;
  oldparent: TWinControl;
begin
  oldparent:=parent;
  inherited SetParent(NewParent);

  if (NewParent<>nil) and (oldparent=nil) then
  begin
    hglrc:=wglCreateContext(canvas.handle);

    if hglrc=0 then
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

      hglrc:=wglCreateContext(canvas.handle);
    end;

    if hglrc=0 then
      raise exception.create('failure creating opengl window');



//    initgl;
  //  Resize;
  end;
end;

procedure TMemDisplay.resize;
begin
  //
//  render;
end;

procedure TMemDisplay.update;
begin
 // render;
end;

procedure TMemDisplay.repaint;
begin
//  render;
end;

procedure TMemDisplay.wndproc(var TheMessage: TLMessage);
begin
  if TheMessage.msg=lm_paint then
    render()
  else
    oldWndProc(TheMessage);

 // render;
end;

procedure TMemDisplay.render;
var
  i,j,w,h: integer;
  maxheight: integer;
  before: TLargeInteger;
  after: TLargeInteger;
  diff: TLargeInteger;
 // map: array of float;
  constantAlpha: float;
  row: single;
  overlay: PCurrentOverlay;



  x,y: single;

  r: trect;

  f: THandle;

  tl,br: TPoint;
  s: string;

begin
  QueryPerformanceCounter(before);
  //render the memory bitmap
  if parent=nil then exit;

  if hasfont=false then
    setupFont;

  wglMakeCurrent(canvas.handle, hglrc);


  glPixelTransferf(GL_ALPHA_SCALE, 0.0);
  glPixelTransferf(GL_ALPHA_BIAS,  1.0);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  glClearIndex(0.0);
  glClear(GL_COLOR_BUFFER_BIT);

  if (fpixelformat=GL_COLOR_INDEX) then
  begin
     {
    glPixelTransferi(GL_MAP_COLOR, GL_TRUE);

    glPixelTransferi(GL_INDEX_OFFSET, 1);


    glPixelMapfv(GL_PIXEL_MAP_I_TO_R, 256, @mapr[0]);
    glPixelMapfv(GL_PIXEL_MAP_I_TO_G, 256, @mapg[0]);
    glPixelMapfv(GL_PIXEL_MAP_I_TO_B, 256, @mapb[0]);

    constantAlpha:=1;

    glPixelMapfv(GL_PIXEL_MAP_I_TO_A, 1, @constantAlpha);

    glPixelTransferi(GL_INDEX_SHIFT, 0);
    glPixelTransferi(GL_INDEX_OFFSET, 0);
    glPixelTransferi(GL_MAP_COLOR, GL_TRUE);    }

  end;
  glDisable(GL_DITHER);




  glShadeModel(GL_FLAT);
  glClearColor(0.0, 0.0, 0.0, 0.5);
  glClearDepth(1.0);
  //glEnable(GL_DEPTH_TEST);
  glDisable(GL_DITHER);
  //glDepthFunc(GL_LEQUAL);
  //glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  glViewport(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluOrtho2D(0, width, height,0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  gluLookAt (0.0, 0.0, 0.0,
             0.0, 1.0, 0.0,
             0.0, 0.0, -1.0);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();

  glViewport(fXpos, fYpos, width, height);
  glPixelZoom(fZoom, -fZoom);
  glRasterPos2f(0,0);




  if p<>nil then
  begin
    maxheight:=size div fPitch;

    maxheight:=min(fypos+height, maxheight); //limit by the height

    row:=fypos / fZoom;
    i:=fpitch*trunc(row);
    if i<0 then i:=0;




    glDrawPixels(fPitch div fPixelByteSize, maxheight, fpixelformat,GL_UNSIGNED_BYTE, p);


    //draw overlays (if visible)
  {  overlay=overlays;
    while overlay do
    begin
    //glDrawPixels(fPitch, maxheight, GL_RGBA,GL_UNSIGNED_BYTE, p);
   // glDrawPixels(fPitch, maxheight, GL_RGBA,GL_UNSIGNED_BYTE, p);

    end;   }


  end;

  //glut

  //todo: Display the pixel values if the zoom factor is big enough
  if hasfont and (fZoom>2) then
  begin
   { canvas.Font.Color:=clred;
    canvas.font.name:='Comic Sans'; }

  {  f:=CreateFont(
        -12,                           // Height
        0,                             // Width
        0,                             // Angle of Rotation
        0,                             // Orientation
        FW_NORMAL,                     // Weight
        0,                             // Italic
        0,                             // Underline
        0,                             // Strike Out
        ANSI_CHARSET,                  // Char Set
        OUT_DEFAULT_PRECIS,            // Precision
        CLIP_DEFAULT_PRECIS,           // Clipping
        DEFAULT_QUALITY,               // Render Quality
        VARIABLE_PITCH or FF_SWISS,    // Pitch & Family
        'MS Sans Serif');              // Font Name       }

//    SelectObject (canvas.handle, f);

   { canvas.Font.Handle:=f;     }


    //glLoadIdentity();
    //glPixelZoom(1, -1);

   // glViewport(0, 0, width, height);
    ;


    //glScalef(20.0, 10.0, 10.0);
       //move this to one time init
    //if wglUseFontOutlines(canvas.handle, 0, 255, 1000,  0.0, 0.1, WGL_FONT_POLYGONS, @agmf) then
    begin

   // glTranslatef(fxPos, fyPos,  -5.0);
    //
//      canvas.font.height:=-trunc(fzoom);

       //go through every visible pixel and render the value

     // glPixelZoom(1, -1);

      glListBase(1000);

      //get the top left pixel
      tl:=GetTopLeftPixelCoordinates;
      //get the bottom right pixel
      br:=GetBottomRightPixelCoordinates;


      glRasterPos2f(0,0);

      for i:=tl.x to br.x do
        for j:=tl.y to br.y do
        begin


          s:=inttostr(i)+','+inttostr(j);
          h:=canvas.TextHeight(s);

          w:=canvas.TextWidth(s);
          x:=i*fZoom+0.5*fZoom-(w/2);
          y:=h+j*fzoom;

          if x<0 then x:=0;
          if y<0 then y:=0;

          glViewport(trunc(fXpos+x), trunc(fYpos-y), width, height);
          glRasterPos2f(0,0);

          glCallLists(length(s), GL_UNSIGNED_BYTE, pchar(s));

        end;






    end
   { else
    begin
      i:=GetLastError;
      if i=0 then beep;
    end; }


  end;

  SwapBuffers(canvas.handle);





  QueryPerformanceCounter(after);

  lastdiff:=after-before;

  inc(totaldiff, lastdiff);
  inc(ticks);

end;

constructor TMemDisplay.Create(TheOwner: TComponent);
var i: integer;
begin
  inherited create(TheOwner);

  setlength(mapr,256);
  for i:=0 to 255 do
    mapr[i]:= (i and 7)/7.0;

  setlength(mapg,256);
  for i:=0 to 255 do
    mapg[i] := ((i and $38)>>3)/7.0;

  setlength(mapb,256);
  for i:=0 to 255 do
    mapb[i] := ((i and $c0)>>6)/3.0;


  oldWndProc:=WindowProc;

  WindowProc:=wndproc;


  fZoom:=32;
  fPixelFormat:=GL_RGBA;




  updater:=TIdleTimer.Create(self);
  updater.interval:=100;
  updater.OnTimer:=updaterevent;



end;

end.

