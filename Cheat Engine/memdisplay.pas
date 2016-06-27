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


type
  TOnDataEvent=function(newAddress: ptruint; PreferedMinimumSize: integer; var newbase: pointer; var newsize: integer): boolean of object;
  //This event will be called when the displayed size is changed, or when the position is moved, and it won't fit in the current block anymore

  TOnRequestTextEvent=function (Address: ptruint): string of object;



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
    fwantedPixelsPerLine: integer; //when the format changes, use this as lead for the new pitch
    fPitch: integer;
    fPixelFormat: integer;
    fPixelByteSize: integer; //the number of bytes one pixel exists of (I do not support monochrome...)

    isDragging: boolean;
    DragOrigin: TPoint;
    PosOrigin: TPoint;
    AddressOrigin: ptruint;
    dragAddress: boolean; //Use this to change the address instead (horizontal movement only)

    fMaxCharCount: integer; //defines how big the font will be

    fOnData: TOnDataEvent;
    fOnRequestText: TOnRequestTextEvent;

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
    procedure setMaxCharCount(v: integer);
  public
    totaldiff: qword;
    ticks: integer;
    lastdiff: qword;
    function MoveTo(xpos, ypos: integer): boolean;
    procedure setFormat(format: integer);
    procedure setPitch(pitch: integer);
    procedure setPixelsPerLine(ppl: integer);
    procedure setPointer(address: ptruint); overload;
    procedure setPointer(address: ptruint; p: pointer; size: integer); overload;
    procedure update; override;
    procedure repaint; override;
    procedure render;

    function getAddressFromScreenPosition(x: integer; y: integer): ptruint;

    function GetTopLeftPixelCoordinates: TPoint; //returns the unzoomed coordinates of the selected pixel
    function GetBottomRightPixelCoordinates: TPoint;
    function getTopLeftAddress: ptruint;


    property onData: TOnDataEvent read fOnData write fOnData;
    property onRequestText: TOnRequestTextEvent read fOnRequestText write fOnRequestText;
    property MaxCharCount: integer read fMaxCharCount write setMaxCharCount;

    property zoom: single read fZoom;
    //property getOffset: integer;

    constructor Create(TheOwner: TComponent); override;
  published
    property OnDblClick;
  end;


implementation

resourcestring
  rsOnDataReturnedATooSmallMemoryRegion = 'OnData returned a too small memory region. It should have returned false instead';
  rsFailureCreatingOpenglWindow = 'failure creating opengl window';

procedure TMemDisplay.setPointer(address: ptruint);
var newp: pointer;
    newsize: integer;
begin
  self.address:=address;

  if assigned(fOnData) and fOnData(address,size,newp,newsize) then
  begin
    p:=newp;
    size:=newsize;

    LimitCoordinates; //recheck with the new ypos. (in case of size change (end of buf?))

    render;
  end
  else
  begin
    self.p:=nil;
    size:=-1;
  end;




end;

procedure TMemDisplay.setPointer(address: ptruint; p: pointer; size: integer);
var newp: pointer;
    newsize: integer;
begin
  self.address:=address;
  self.p:=p;
  self.size:=size;

  if assigned(fOnData) and fOnData(address,size,newp,newsize) then
  begin
    p:=newp;
    size:=newsize;

    LimitCoordinates; //recheck with the new ypos. (in case of size change (end of buf?))

    render;
  end

end;

procedure TMemDisplay.setFormat(format: integer);
var oldaddress: integer;
begin
  oldaddress:=getTopLeftAddress;
  fPixelFormat:=format;


  case fpixelformat of
    GL_RGB: fPixelByteSize:=3;
    GL_RGBA: fPixelByteSize:=4;
    GL_LUMINANCE_ALPHA: fPixelByteSize:=3;
    else
      fPixelByteSize:=1;
  end;


  setPixelsPerLine(fwantedPixelsPerLine);

  fxpos:=0;
  fypos:=0;
  setPointer(oldaddress);
  LimitCoordinates;
  render;
end;

procedure TMemDisplay.setPixelsPerLine(ppl: integer);
begin
  fwantedPixelsPerLine:=ppl;
  if ppl<>0 then
    setPitch(fPixelByteSize*ppl);

end;

procedure TMemDisplay.setPitch(pitch: integer);
var oldaddress,newaddress: ptruint;
  diff: int64;
begin
  oldaddress:=getTopLeftAddress;
  fPitch:=pitch;
  if fPitch<=0 then
    fPitch:=1;


  newaddress:=getTopLeftAddress;

  //calculate how much to move to get to the new position

  diff:=newaddress-oldaddress;

  fYpos:=trunc(fYpos-(diff/fPitch)*fZoom);

  fXpos:=trunc(fXpos+(diff mod fPitch)*fzoom);

  LimitCoordinates;

  render;



end;

procedure TMemDisplay.updaterevent(sender: TObject);
begin
  render;
end;


function TMemDisplay.getAddressFromScreenPosition(x: integer; y: integer): ptruint;
var c: tpoint;
begin


  c.x:=trunc((-fxpos+x) / fzoom);
  c.y:=trunc((fypos+y)/fzoom);

  result:=self.address+c.y*fPitch+c.x*fPixelByteSize;

end;


function TMemdisplay.getTopLeftAddress: ptruint;
var c: tpoint;
begin
  c:=GetTopLeftPixelCoordinates;

  result:=self.address+c.y*fPitch+c.x*fPixelByteSize;


//  address:=
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
  if fPixelByteSize<=0 then exit; //not yet initialized

  visiblepixelwidth:=trunc(Width / fZoom);
  visiblerows:=trunc(height / fzoom);
  bytesperrow:=fPitch;
  pixelsperrow:=bytesperrow div fPixelByteSize;


  if (-fxpos / fzoom) >(pixelsperrow)-(visiblepixelwidth div 2) then
    fxpos:=-trunc(((pixelsperrow)-(visiblepixelwidth div 2)) * fZoom);

  if fXpos>0 then
    fXpos:=0;



  a:=address;
  preferedsize:=(visiblerows+16)*bytesperrow;

  //check if the ypos is outside the allowed region

  row:=fYpos / fZoom;
  if (row>0) and (row*bytesperrow>(size-(visiblerows*bytesperrow))) then //-(visiblerows*bytesperrow)) then //outside
  begin


    a:=getTopLeftAddress;
//    inc(a,row*bytesperror-(visiblerows*bytesperrow));

    if assigned(fOnData) and fOnData(a,preferedsize,newp,newsize) then
    begin
      if newsize<preferedsize then raise exception.create(rsOnDataReturnedATooSmallMemoryRegion);

      address:=a;
      p:=newp;
      size:=newsize;

      //set the new ypos to the new topleft position
      f:=fypos / fZoom;
      f:=abs(f-trunc(f));
      f:=f*fZoom;

      fypos:=trunc(fzoom-f);
      if isDragging then
        RecenterDrag;

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
    a:=self.address-ceil(-(fypos/fzoom)) *bytesperrow;



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
      if isDragging then
        RecenterDrag;

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

    dragaddress:=ssCtrl in shift;
    if dragAddress then
      addressOrigin:=address;
  end;

end;

procedure TMemDisplay.MouseMove(Shift: TShiftState; X, Y: Integer);
var a: ptruint;
  newp: pointer;
  newsize: integer;
begin
  if isDragging then
  begin
    if dragaddress then
    begin
      //move the address by the difference in X position


      a:=addressOrigin-trunc((PosOrigin.x-(DragOrigin.x-x))/fzoom)*fPixelByteSize;
      if assigned(fOnData) and fOnData(a,size,newp,newsize) then
      begin
        address:=a;
        p:=newp;
        size:=newsize;

        LimitCoordinates //recheck with the new ypos. (in case of size change (end of buf?))
      end;


      render;

    end
    else
      MoveTo(PosOrigin.x-(DragOrigin.x-x), PosOrigin.y+(DragOrigin.y-y));

  end;
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

      if ssShift in Shift then
        fZoom:=fZoom * 1.2
      else
        fZoom:=fZoom * 2;

      fXpos:=trunc(-oldx*fZoom+(MousePos.x));

      newx:=(-fXpos+(MousePos.x))/fZoom;

      fypos:=trunc(oldy*fZoom-(MousePos.y));
      newy:=(fYpos+(MousePos.y))/fZoom;



    end;
  end
  else if WheelDelta<0 then
  begin
    if fZoom>0.2 then
    begin
      //zoom out
      if ssShift in Shift then
        fZoom:=fZoom / 1.2
      else
        fZoom:=fZoom / 2;

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

procedure TMemDisplay.setMaxCharcount(v: integer);
begin
  fMaxCharCount:=v;
  setupFont;
  render;
end;

procedure TMemDisplay.setupFont;
var z: integer;
begin


  z:=floor(fZoom/((fMaxCharCount+1)/2));
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
      raise exception.create(rsFailureCreatingOpenglWindow);



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
  i,j,k,w,h: integer;
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
  s: tstringlist;

begin
  QueryPerformanceCounter(before);

  //render the memory bitmap
  if parent=nil then exit;

  if hasfont=false then
    setupFont;

  if (size<=0) or (p=nil) then
    exit;

  wglMakeCurrent(canvas.handle, hglrc);


  glPixelTransferf(GL_ALPHA_SCALE, 0.0);
  glPixelTransferf(GL_ALPHA_BIAS,  1.0);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  glClearIndex(0.0);
  glClear(GL_COLOR_BUFFER_BIT);

  if (fpixelformat=GL_COLOR_INDEX) then
  begin

    glPixelTransferi(GL_MAP_COLOR, GL_TRUE);

    glPixelTransferi(GL_INDEX_OFFSET, 1);


    glPixelMapfv(GL_PIXEL_MAP_I_TO_R, 256, @mapr[0]);
    glPixelMapfv(GL_PIXEL_MAP_I_TO_G, 256, @mapg[0]);
    glPixelMapfv(GL_PIXEL_MAP_I_TO_B, 256, @mapb[0]);

    constantAlpha:=1;

    glPixelMapfv(GL_PIXEL_MAP_I_TO_A, 1, @constantAlpha);

    glPixelTransferi(GL_INDEX_SHIFT, 0);
    glPixelTransferi(GL_INDEX_OFFSET, 0);
    glPixelTransferi(GL_MAP_COLOR, GL_TRUE);

  end
  else
  begin
    glPixelTransferi(GL_MAP_COLOR, GL_FALSE);
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

    maxheight:=min(ceil((height+fypos) / fzoom), maxheight ); //limit by the height

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
  if hasfont and (fZoom>8) and (assigned(fOnRequestText)) then //at least 8 pixels...
  begin


    glListBase(1000);

    //get the top left pixel
    tl:=GetTopLeftPixelCoordinates;
    //get the bottom right pixel
    br:=GetBottomRightPixelCoordinates;


    glRasterPos2f(0,0);
    s:=tstringlist.create;

    for i:=tl.x to br.x do
      for j:=tl.y to br.y do
      begin
        s.Text:=fOnRequestText(address+(j*fpitch)+i*fPixelByteSize);

        for k:=0 to s.Count-1 do
        begin
          h:=canvas.TextHeight(s[k]);

          w:=canvas.TextWidth(s[k]);
          x:=i*fZoom+0.5*fZoom-(w/2);
          y:=h*(k+1)+j*fzoom;//+0.5*fZoom-(h/2);

          if x<0 then x:=0;
          if y<0 then y:=0;

          glViewport(trunc(fXpos+x), trunc(fYpos-y), width, height);
          glRasterPos2f(0,0);

          glCallLists(length(s[k]), GL_UNSIGNED_BYTE, pchar(s[k]));

        end;

      end;




    s.free;
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

  fMaxCharCount:=5;

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


  //some default inits
  fZoom:=32;
  setFormat(GL_RGBA);
  setPitch(128);


  updater:=TIdleTimer.Create(self);
  updater.interval:=100;
  updater.OnTimer:=updaterevent;



end;

end.

