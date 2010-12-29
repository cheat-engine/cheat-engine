unit disassemblerviewunit;

{$MODE Delphi}

{
Disassemblerview is a component that displays the memory using the disassembler routines

requirements:
display disassembled lines split up into sections, resizable with a section bar
selecting one or more lines
keyboard navigation
show function names before each function

extra classes:
Lines

Lines contain he disassembled address and the description of that line

}

interface

uses jwawindows, windows, sysutils, LCLIntf,forms, classes, controls, comctrls, stdctrls, extctrls, symbolhandler,
     cefuncproc, NewKernelHandler, graphics, disassemblerviewlinesunit, disassembler,
     math, lmessages, menus,commctrl, dissectcodethread;



type TShowjumplineState=(jlsAll, jlsOnlyWithinRange);     



type TDisassemblerview=class(TPanel)
  private
    statusinfo: TPanel;
    statusinfolabel: TLabel;
    header: THeaderControl;
    previousCommentsTabWidth: integer;

    scrollbox: TScrollbox; //for the header
    verticalscrollbar: TScrollbar;
    disassembleDescription: TPanel;

    disCanvas: TPaintbox;
    isUpdating: boolean;

    fTotalvisibledisassemblerlines: integer;
    disassemblerlines: tlist;

    offscreenbitmap: TBitmap;

    fSelectedAddress: ptrUint; //normal selected address
    fSelectedAddress2: ptrUint; //secondary selected address (when using shift selecting)
    fTopAddress: ptrUint; //address to start disassembling from
    fShowJumplines: boolean; //defines if it should draw jumplines or not
    fShowjumplineState: TShowjumplineState;


   // fdissectCode: TDissectCodeThread;

    lastupdate: dword;
    destroyed: boolean;
    procedure updateScrollbox;
    procedure scrollboxResize(Sender: TObject);

    //-scrollbar-
    procedure scrollbarChange(Sender: TObject);
    procedure scrollbarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure scrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    procedure headerSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure headerSectionTrack(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);


    procedure OnLostFocus(sender: TObject);
    procedure MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DisCanvasPaint(Sender: TObject);
    procedure DisCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DisCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetPopupMenu(p: Tpopupmenu);
    function getPopupMenu: Tpopupmenu; //hidden on purpose
    procedure setSelectedAddress(address: ptrUint);
    procedure setSelectedAddress2(address: ptrUint);
    procedure setTopAddress(address: ptrUint);
    function getOnDblClick: TNotifyEvent;
    procedure setOnDblClick(x: TNotifyEvent);
    procedure renderJumpLines;
    procedure setJumpLines(state: boolean);
    procedure setJumplineState(state: tshowjumplinestate);
    procedure synchronizeDisassembler;
  protected
    procedure HandleSpecialKey(key: word);
    procedure WndProc(var msg: TMessage); override;
    procedure DoEnter; override;
    procedure DoExit; override;

  published
    property OnKeyDown;
    property OnDblClick: TNotifyEvent read getOnDblClick write setOnDblClick;

  public
    colors: TDisassemblerViewColors;

    procedure reinitialize; //deletes the assemblerlines

    procedure BeginUpdate; //stops painting until endupdate
    procedure EndUpdate;
    procedure Update; override;
    procedure setCommentsTab(state: boolean);

    function getheaderWidth(headerid: integer): integer;
    procedure setheaderWidth(headerid: integer; size: integer);

    property Totalvisibledisassemblerlines: integer read fTotalvisibledisassemblerlines;
    property PopupMenu: TPopupMenu read getPopupMenu write SetPopupMenu;
    property SelectedAddress: ptrUint read fSelectedAddress write setSelectedAddress;
    property SelectedAddress2: ptrUint read fSelectedAddress2 write setSelectedAddress2;
    property TopAddress: ptrUint read fTopAddress write setTopAddress;
    property ShowJumplines: boolean read fShowJumplines write setJumpLines;
    property ShowJumplineState: TShowJumplineState read fShowjumplinestate write setJumplineState;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
end;


implementation

procedure TDisassemblerview.setJumplineState(state: tshowjumplinestate);
begin
  fShowjumplineState:=state;
  update;
end;

procedure TDisassemblerview.setJumpLines(state: boolean);
begin
  fShowJumplines:=state;
  update;
end;

function TDisassemblerview.getOnDblClick: TNotifyEvent;
begin
  if discanvas<>nil then
    result:=disCanvas.OnDblClick;
end;

procedure TDisassemblerview.setOnDblClick(x: TNotifyEvent);
begin
  if discanvas<>nil then
    disCanvas.OnDblClick:=x;
end;

function TDisassemblerview.getheaderWidth(headerid: integer): integer;
begin
  if header<>nil then
    result:=header.Sections[headerid].width;
end;

procedure TDisassemblerview.setheaderWidth(headerid: integer; size: integer);
begin
  if header<>nil then
    header.Sections[headerid].width:=size;
end;

procedure TDisassemblerview.setCommentsTab(state:boolean);
begin
  if header<>nil then
  begin
    if not state then
    begin
      previousCommentsTabWidth:=header.Sections[3].Width;
      header.Sections[3].MinWidth:=0;
      header.Sections[3].Width:=0;
      header.Sections[3].MaxWidth:=0;
    end
    else
    begin
      header.Sections[3].MaxWidth:=10000;
      if previousCommentsTabWidth>0 then
        header.Sections[3].Width:=previousCommentsTabWidth;

      header.Sections[3].MinWidth:=5;
    end;

    update;
    headerSectionResize(header, header.Sections[3]);
  end;
end;

procedure TDisassemblerview.setTopAddress(address: ptrUint);
begin
  fTopAddress:=address;
  fSelectedAddress:=address;
  fSelectedAddress2:=address;
  update;
end;

procedure TDisassemblerview.setSelectedAddress2(address: ptrUint);
begin
  //just set the address and refresh, no need to do anything special
  fSelectedAddress2:=address;
  update;
end;

procedure TDisassemblerview.setSelectedAddress(address: ptrUint);
var i: integer;
    found: boolean;
begin
  fSelectedAddress:=address;
  fSelectedAddress2:=address;

  if (fTotalvisibledisassemblerlines>1) and (InRangeX(fSelectedAddress, Tdisassemblerline(disassemblerlines[0]).address, Tdisassemblerline(disassemblerlines[fTotalvisibledisassemblerlines-2]).address)) then
  begin
    //in range
    found:=false;
    for i:=0 to fTotalvisibledisassemblerlines-1 do
      if address=Tdisassemblerline(disassemblerlines[i]).address then
      begin
        found:=true;
        break;
      end;

    //not in one of the current lines. Looks like the disassembler order is wrong. Fix it by setting it to the top address
    if not found then
      fTopAddress:=address;

  end else fTopAddress:=address;


  update;
end;

procedure TDisassemblerview.MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
//  messagebox(0,'scroll','',0);
  if wheelDelta>0 then
    fTopAddress:=previousopcode(fTopAddress)
  else
    disassemble(fTopAddresS);

  update;

end;


function TDisassemblerview.getPopupMenu: Tpopupmenu;
begin
  if discanvas<>nil then
    result:=discanvas.PopupMenu;
end;

procedure TDisassemblerview.SetPopupMenu(p: tpopupmenu);
begin
  if discanvas<>nil then
    discanvas.PopupMenu:=p;
end;

procedure TDisassemblerview.HandleSpecialKey(key: word);
var i: integer;
    shiftispressed: boolean;
  //  ctrlispressed: boolean;
  //  altispressed: boolean;
begin
 // messagebox(0,'secial key','',0);
  beginupdate;

  shiftispressed:=GetBit(15, GetKeyState(VK_SHIFT))=1;
 // ctrlispressed:=GetBit(15, GetKeyState(VK_CONTROL))=1;
 // altispressed:=GetBit(15, GetKeyState(VK_MENU))=1;

  case key of
    VK_UP:
    begin
      fSelectedAddress:=previousopcode(fSelectedAddress);
      if (fSelectedAddress<fTopAddress) then
        fTopAddress:=fSelectedAddress;
    end;

    VK_DOWN:
    begin
      disassemble(fSelectedAddress);
      if (fTotalvisibledisassemblerlines-2)>0 then
      begin
        if (fSelectedAddress >= TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-2]).address) then
        begin
          disassemble(fTopAddress);
          update;
          fSelectedAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-2]).address;
        end;
      end;

    end;

    VK_LEFT:
      dec(fTopAddress);

    VK_RIGHT:
      inc(fTopAddress);

    vk_prior:
    begin
      if fSelectedAddress>TDisassemblerLine(disassemblerlines[0]).address then fSelectedAddress:=TDisassemblerLine(disassemblerlines[0]).address else
      begin
        for i:=0 to fTotalvisibledisassemblerlines-2 do
          fTopAddress:=previousopcode(fTopAddress);  //go 'numberofaddresses'-1 times up
        fSelectedAddress:=0;

        update;
        fSelectedAddress:=TDisassemblerLine(disassemblerlines[0]).address;
      end;
    end;

    vk_next:
    begin
      if fSelectedAddress<TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-2]).address then
        fSelectedAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-2]).address
      else
      begin
        fTopAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-2]).address;
        fSelectedAddress:=0;
        update;
        fSelectedAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-2]).address;

      end;
    end;

  end;

  //check if shift is pressed
  if not shiftispressed then
    fSelectedAddress2:=fSelectedAddress;

  endupdate;
end;


procedure TDisassemblerview.WndProc(var msg: TMessage);
{$ifdef cpu64}
type
  TWMKey2 = record
    Msg: dword;
    filler: dword; //fpc 2.4.1 has a broken tmessage structure
    CharCode: Word;
    Unused: Word;
    KeyData: Longint;
    Result: LRESULT;
  end;
var
    y: ^TWMKey2;
    x: TWMKey2;
{$else}
var
    y: ^TWMKey;
    x: TWMKey;
{$endif}

begin
  y:=@msg;
  x:=y^;

  //outputdebugstring(inttohex(msg.msg,8));

  if msg.msg=WM_MOUSEWHEEL then
  begin
//    messagebox(0,'wm_mousewheel','',0);

  end;
  if msg.Msg=CN_KEYDOWN then
  begin
    // messagebox(0,pchar('1 : '+inttohex(ptrUint(@msg.msg),8)+' - '+inttohex(ptrUint(@msg.wparam),8)+' - '+inttohex(ptrUint(@msg.lparam),8)  ),'',0);

    // messagebox(0,pchar('CN_KEYDOWN : '+inttohex(ptrUint(@y^.CharCode),8)+' - '+inttohex(ptrUint(@msg.wparam),8)+' - '+inttohex(msg.wparam,8)),'',0);
    case x.CharCode of
      VK_LEFT,   //direction keys
      VK_RIGHT,
      VK_UP,
      VK_DOWN,
      VK_NEXT,
      VK_PRIOR,
      VK_HOME,
      VK_END: HandleSpecialKey(x.CharCode);

      else inherited WndProc(msg);
    end;
  end else inherited WndProc(msg);
end;

procedure TDisassemblerview.DoEnter;
begin
  inherited DoEnter;
  update;
end;

procedure TDisassemblerview.DoExit;
begin
  inherited DoExit;
  update;
end;


procedure TDisassemblerview.DisCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in shift then
    DisCanvas.OnMouseDown(self,mbleft,shift,x,y);
end;

procedure TDisassemblerview.DisCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
    found: boolean;
    line: TDisassemblerLine;
begin
  if not Focused then SetFocus;


  if (y<0) or (y>discanvas.Height) then exit; //moved out of range

  //find the selected line
  found:=false;
  for i:=0 to fTotalvisibledisassemblerlines-1 do
  begin
    line:=disassemblerlines[i];
    if (y>=line.getTop) and (y<(line.getTop+line.getHeight)) then
    begin
      //found the selected line
      found:=true;
      break;
    end;
  end;
  if not found then exit; //not found, which is weird, but whatever...


  if line.address<>fSelectedAddress then
  begin
    //not the selected address

    fSelectedAddress:=line.address;

    //set the secondary address to the same as the first if shift isn't pressed
    if not (ssShift in shift) then
      fSelectedAddress2:=fSelectedAddress;
  end;

  
  //now update the RWAddress
  update;
end;

procedure TDisassemblerview.DisCanvasPaint(Sender: TObject);
var cr: Trect;
begin
  cr:=discanvas.Canvas.ClipRect;
  discanvas.Canvas.CopyRect(cr,offscreenbitmap.Canvas,cr);
end;

procedure TDisassemblerview.renderJumpLines;
{
will render the lines of visible jumps
pre: must be called after the disassemblerlines have been created and configured
}
var
  currentline,linex: TDisassemblerLine;
  i,j: integer;
  address: ptrUint;

  found: boolean;
  jumplineoffset: integer;
begin
  jumplineoffset:=4;

  for i:=0 to fTotalvisibledisassemblerlines-1 do
  begin
    currentline:=disassemblerlines[i];
    if currentline.isJumpOrCall(address) then
    begin
      for j:=0 to fTotalvisibledisassemblerlines-1 do
      begin
        linex:=disassemblerlines[j];
        if linex.address=address then
        begin
          currentline.drawJumplineTo(linex.instructionCenter, jumplineoffset);
          found:=true;
          break;
        end else
        if (j>0) and (linex.address>address) and (TDisassemblerline(disassemblerlines[j-1]).address<address) then //we past it...
        begin
          currentline.drawJumplineTo(linex.gettop, jumplineoffset);
          found:=true;
          break;
        end;

      end;

      if fShowjumplineState=jlsAll then
      begin
        if not found then
        begin
          //not in the visible region
          if currentline.address>address then
          begin
            //line to the top
            currentline.drawJumplineTo(0, jumplineoffset,false);
          end
          else
          begin
            //line to the bottom
            currentline.drawJumplineTo(offscreenbitmap.Height-1, jumplineoffset,false);
          end;
        end;
      end;

      inc(jumplineoffset,2);
    end;
  end;
end;

procedure TDisassemblerview.synchronizeDisassembler;
begin
  visibleDisassembler.showmodules:=symhandler.showModules;
  visibleDisassembler.showsymbols:=symhandler.showsymbols;
end;

procedure TDisassemblerview.update;
{
fills in all the lines according to the current state
and then renders the lines to the offscreen bitmap
finally the bitmap is rendered by the onpaint event of the discanvas
}
var
  currenttop:integer; //determines the current position that lines are getting renderesd
  i: integer;
  currentline: TDisassemblerLine;
  currentAddress: ptrUint;

  selstart, selstop: ptrUint;
  description: string;
  x: ptrUint;
begin
  inherited update;

  if destroyed then exit;

  synchronizeDisassembler;

  //if gettickcount-lastupdate>50 then
  begin
    if (not symhandler.isloaded) and (not symhandler.haserror) then
    begin
      if processid>0 then
        statusinfolabel.Caption:='Symbols are being loaded'
      else
        statusinfolabel.Caption:='Please open a process first';

    end
    else
    begin
      if symhandler.haserror then
        statusinfolabel.Font.Color:=clRed
      else
        statusinfolabel.Font.Color:=clWindowText;

      statusinfolabel.Caption:=symhandler.getnamefromaddress(TopAddress);
    end;

    //initialize bitmap dimensions
    if discanvas.width>scrollbox.HorzScrollBar.Range then
      offscreenbitmap.Width:=discanvas.width
    else
      offscreenbitmap.Width:=scrollbox.HorzScrollBar.Range;
    offscreenbitmap.Height:=discanvas.Height;

    //clear bitmap
    offscreenbitmap.Canvas.Brush.Color:=clBtnFace;
    offscreenbitmap.Canvas.FillRect(rect(0,0,offscreenbitmap.Width, offscreenbitmap.Height));

    currenttop:=0;
    i:=0;

    currentAddress:=fTopAddress;
    selstart:=minX(fSelectedAddress,fSelectedAddress2);
    selstop:=maxX(fSelectedAddress,fSelectedAddress2);

    offscreenbitmap.Canvas.Font:=font;

    while currenttop<offscreenbitmap.Height do
    begin
      while i>=disassemblerlines.Count do //add a new line
        disassemblerlines.Add(TDisassemblerLine.Create(offscreenbitmap, header.Sections, @colors));

      currentline:=disassemblerlines[i];


      currentline.renderLine(currentAddress,currenttop, inrangeX(currentAddress,selStart,selStop), currentAddress=fSelectedAddress);

      inc(currenttop, currentline.getHeight);
      inc(i);
    end;

    fTotalvisibledisassemblerlines:=i;

    x:=fSelectedAddress;

    disassemble(x,description);
    if disassembleDescription.caption<>description then disassembleDescription.caption:=description;


    if ShowJumplines then
      renderjumplines;


    if not isupdating then
      disCanvas.Repaint;
    lastupdate:=gettickcount;
  end;


end;

procedure TDisassemblerview.reinitialize;
var i: integer;
begin
  fTotalvisibledisassemblerlines:=0;
  if disassemblerlines<>nil then
  begin
    for i:=0 to disassemblerlines.Count-1 do
      TDisassemblerLine(disassemblerlines[i]).free;

    disassemblerlines.Clear;
  end;

end;

procedure TDisassemblerview.BeginUpdate;
begin
  isUpdating:=true;
end;

procedure TDisassemblerview.EndUpdate;
begin
  isUpdating:=false;
  update;
end;

procedure TDisassemblerview.updateScrollbox;
var x: integer;
begin
  scrollbox.OnResize:=nil;

  x:=(header.Sections[header.Sections.Count-1].Left+header.Sections[header.Sections.Count-1].Width);

  header.width:=max(x, scrollbox.ClientWidth);
  discanvas.width:=max(x, scrollbox.ClientWidth);

  scrollbox.VertScrollBar.Visible:=false;




  scrollbox.HorzScrollBar.Range:=(x+scrollbox.HorzScrollBar.Page)-scrollbox.clientwidth;
  scrollbox.HorzScrollBar.Visible:=true;

  update;



  scrollbox.OnResize:=scrollboxResize;
end;

procedure TDisassemblerView.scrollboxResize(Sender: TObject);
begin
  updatescrollbox;
end;

//scrollbar
procedure TDisassemblerView.scrollbarChange(Sender: TObject);
begin
  update;
end;

procedure TDisassemblerView.scrollbarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GetFocus;
end;

procedure TDisassemblerView.scrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var x: integer;
    found: boolean;
    temp: string;
    delta: integer;
    i: integer;
begin
  beginupdate;
  
  if scrollcode=sctrack then
  begin
    delta:=scrollpos-50;

    if delta>0 then
    begin
      for i:=0 to delta do
        scrollBarScroll(Sender,scLineDown, scrollpos);
    end
    else
    begin
      for i:=delta to 0 do
        scrollBarScroll(Sender,scLineUp, scrollpos);
    end;
  end;


  case scrollcode of
    scLineUp:   fTopAddress:=previousopcode(fTopAddress);
    scLineDown:
    begin
      found:=false;

      for x:=0 to fTotalvisibledisassemblerlines-1 do
        if fTopAddress<>TDisassemblerLine(disassemblerlines[x]).address then
        begin
          fTopAddress:=TDisassemblerLine(disassemblerlines[x]).address;
          found:=true;
          break;
        end;

      if not found then //disassemble
        disassemble(fTopAddress,temp);
    end;

    scPageDown:
    begin
      if TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-1]).address=fTopAddress then
      begin
        for x:=0 to fTotalvisibledisassemblerlines-1 do
          disassemble(fTopAddress,temp);
      end
      else fTopAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-1]).address;
    end;

    scPageUp:
    begin
      for x:=0 to fTotalvisibledisassemblerlines-2 do
        fTopAddress:=previousopcode(fTopAddress);  //go 'numberofaddresses'-1 times up
    end;


  end;

  scrollpos:=50;      //i dont want the slider to work 100%

  endupdate;
  SetFocus;
end;

//header
procedure TDisassemblerview.headerSectionTrack(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
begin
  updatescrollbox;
end;

procedure TDisassemblerview.headerSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  updatescrollbox;
end;

procedure TDisassemblerView.OnLostFocus(sender: TObject);
begin
  self.SetFocus;
end;


destructor TDisassemblerview.destroy;
begin
  destroyed:=true;

  reinitialize;
  if disassemblerlines<>nil then
    freeandnil(disassemblerlines);

  if offscreenbitmap<>nil then
    freeandnil(offscreenbitmap);

  if disCanvas<>nil then
    freeandnil(disCanvas);

  if header<>nil then
    freeandnil(header);

  if scrollbox<>nil then
    freeandnil(scrollbox);


  if disassembleDescription<>nil then
    freeandnil(disassembleDescription);

  if statusinfolabel<>nil then
    freeandnil(statusinfolabel);

  if statusinfo<>nil then
    freeandnil(statusinfo);


  inherited destroy;
end;

constructor TDisassemblerview.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  tabstop:=true;
  DoubleBuffered:=true;
  AutoSize:=false;

  statusinfo:=tpanel.Create(self);
  with statusinfo do
  begin
    ParentFont:=false;
    align:=alTop;
    bevelInner:=bvLowered;
    height:=19;
    parent:=self;
   // color:=clYellow;
  end;

  statusinfolabel:=TLabel.Create(self);
  with statusinfolabel do
  begin
    parentfont:=false;
    align:=alClient;
    Alignment:=taCenter;
    autosize:=false;
    //transparent:=false;
    parent:=statusinfo;
  end;

  disassembleDescription:=Tpanel.Create(self);
  with disassembleDescription do
  begin
    align:=alBottom;
    height:=17;
    bevelInner:=bvLowered;
    bevelOuter:=bvLowered;
    Color:=clWhite;

    ParentFont:=false;
    Font.Charset:=DEFAULT_CHARSET;
    Font.Color:=clBtnText;
    Font.Height:=-11;
    Font.Name:='Courier';
    Font.Style:=[];

    parent:=self;


  end;

  verticalscrollbar:=TScrollBar.Create(self);


  with verticalscrollbar do
  begin
    align:=alright;
    kind:=sbVertical;
    pagesize:=2;
    position:=50;

    parent:=self;
    OnEnter:=OnLostFocus;
    OnChange:=scrollbarChange;
    OnKeyDown:=scrollbarKeyDown;
    OnScroll:=scrollBarScroll;
  end;

  scrollbox:=TScrollbox.Create(self);
  with scrollbox do
  begin
    DoubleBuffered:=true;
    align:=alClient;


//    bevelInner:=bvNone;
//    bevelOuter:=bvNone;
    Borderstyle:=bsNone;
    HorzScrollBar.Tracking:=true;
    VertScrollBar.Visible:=false;

    scrollbox.OnResize:=scrollboxResize;
    parent:=self;
    OnEnter:=OnLostFocus;
    //OnMouseWheel:=MouseScroll;
  end;


  header:=THeaderControl.Create(self);



  with header do
  begin
    top:=0;
    height:=20;
    OnSectionResize:=headerSectionResize;
    OnSectionTrack:=headerSectionTrack;
    parent:=scrollbox;
    onenter:=OnLostFocus;
    //header.Align:=alTop;
    header.ParentFont:=false;
  end;



  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=50;
    Text:='Address';
    Width:=80;
   end;

  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=50;
    Text:='Bytes';
    Width:=140;
  end;

  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=50;
    Text:='Opcode';
    Width:=200;
  end;

  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=5;
    Text:='Comment';
    AutoSize:=true;

    Width:=100;

  end;



  disCanvas:=TPaintbox.Create(self);
  with disCanvas do
  begin
    top:=header.Top+header.height;
    ParentFont:=true; //False;

    height:=scrollbox.ClientHeight-header.height;
    anchors:=[akBottom, akLeft, akTop, akRight];


    parent:=scrollbox;
    OnPaint:=DisCanvasPaint;
    OnMouseDown:=DisCanvasMouseDown;
    OnMouseMove:=DisCanvasMouseMove;
    OnMouseWheel:=mousescroll;
  end;

  offscreenbitmap:=Tbitmap.Create;


  disassemblerlines:=TList.Create;

  fShowjumplineState:=jlsOnlyWithinRange;
  fShowJumplines:=true;

  self.OnMouseWheel:=mousescroll;


  //setup the default colors:
  self.colors[csNormal].backgroundcolor:=clBtnFace;
  self.colors[csNormal].normalcolor:=clWindowText;
  self.colors[csNormal].registercolor:=clRed;
  self.colors[csNormal].symbolcolor:=clGreen;
  self.colors[csNormal].hexcolor:=clBlue;

  self.colors[csHighlighted].backgroundcolor:=clHighlight;
  self.colors[csHighlighted].normalcolor:=clHighlightText;
  self.colors[csHighlighted].registercolor:=clRed;
  self.colors[csHighlighted].symbolcolor:=clLime;
  self.colors[csHighlighted].hexcolor:=clYellow;

  self.colors[csSecondaryHighlighted].backgroundcolor:=clGradientActiveCaption;
  self.colors[csSecondaryHighlighted].normalcolor:=clHighlightText;
  self.colors[csSecondaryHighlighted].registercolor:=clRed;
  self.colors[csSecondaryHighlighted].symbolcolor:=clLime;
  self.colors[csSecondaryHighlighted].hexcolor:=clYellow;

  self.colors[csBreakpoint].backgroundcolor:=clRed;
  self.colors[csBreakpoint].normalcolor:=clBlack;
  self.colors[csBreakpoint].registercolor:=clGreen;
  self.colors[csBreakpoint].symbolcolor:=clLime;
  self.colors[csBreakpoint].hexcolor:=clBlue;

  self.colors[csHighlightedbreakpoint].backgroundcolor:=clGreen;
  self.colors[csHighlightedbreakpoint].normalcolor:=clWhite;
  self.colors[csHighlightedbreakpoint].registercolor:=clRed;
  self.colors[csHighlightedbreakpoint].symbolcolor:=clLime;
  self.colors[csHighlightedbreakpoint].hexcolor:=clBlue;

  self.colors[csSecondaryHighlightedbreakpoint].backgroundcolor:=clGreen;
  self.colors[csSecondaryHighlightedbreakpoint].normalcolor:=clWhite;
  self.colors[csSecondaryHighlightedbreakpoint].registercolor:=clRed;
  self.colors[csSecondaryHighlightedbreakpoint].symbolcolor:=clLime;
  self.colors[csSecondaryHighlightedbreakpoint].hexcolor:=clBlue;

  scrollbox.AutoSize:=false;
  scrollbox.AutoScroll:=false;

end;



end.

