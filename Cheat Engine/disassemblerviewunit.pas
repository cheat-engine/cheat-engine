unit disassemblerviewunit;
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

uses windows,forms, classes, controls, comctrls, stdctrls, extctrls, symbolhandler,
     cefuncproc, newkernelhandler, graphics, disassemblerviewlinesunit, disassembler,
     math, messages, menus, dissectcodethread;



type TShowjumplineState=(jlsAll, jlsOnlyWithinRange);     
     

type TDisassemblerview=class(tpanel)
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

    fSelectedAddress: dword; //normal selected address
    fSelectedAddress2: dword; //secondary selected address (when using shift selecting)
    fTopAddress: dword; //address to start disassembling from
    fShowJumplines: boolean; //defines if it should draw jumplines or not
    fShowjumplineState: TShowjumplineState;
    fdissectCode: TDissectCodeThread;
    procedure updateScrollbox;
    procedure scrollboxResize(Sender: TObject);

    //-scrollbar-
    procedure scrollbarChange(Sender: TObject);
    procedure scrollbarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure scrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    procedure headerSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure OnLostFocus(sender: TObject);
    procedure DisCanvasPaint(Sender: TObject);
    procedure DisCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DisCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetPopupMenu(p: Tpopupmenu);
    function getPopupMenu: Tpopupmenu;
    procedure wmMouseWheel (var Msg : TWMMouseWheel); message wm_MouseWheel;
    procedure setSelectedAddress(address: dword);
    procedure setTopAddress(address: dword);
    function getOnDblClick: TNotifyEvent;
    procedure setOnDblClick(x: TNotifyEvent);
    procedure renderJumpLines;
    procedure setJumpLines(state: boolean);
    procedure setJumplineState(state: tshowjumplinestate);
    procedure setDissectCodeThread(dc: TdissectCodeThread);
  protected
    procedure HandleSpecialKey(key: word);
    procedure WndProc(var msg: TMessage); override;
    procedure DoEnter; override;
    procedure DoExit; override;

  published
    property OnKeyDown;
    property OnDblClick: TNotifyEvent read getOnDblClick write setOnDblClick;

  public
    procedure BeginUpdate; //stops painting until endupdate
    procedure EndUpdate;
    procedure Update;
    procedure setCommentsTab(state: boolean);

    function getheaderWidth(headerid: integer): integer;
    procedure setheaderWidth(headerid: integer; size: integer);

    property Totalvisibledisassemblerlines: integer read fTotalvisibledisassemblerlines;
    property PopupMenu: TPopupMenu read getPopupMenu write SetPopupMenu;
    property SelectedAddress: dword read fSelectedAddress write setSelectedAddress;
    property SelectedAddress2: dword read fSelectedAddress2;
    property TopAddress: dword read fTopAddress write setTopAddress;
    property ShowJumplines: boolean read fShowJumplines write setJumpLines;
    property ShowJumplineState: TShowJumplineState read fShowjumplinestate write setJumplineState;
    property DissectCode: TDissectCodeThread read fDissectCode write setDissectCodeThread;

    constructor create(AOwner: TComponent); override;
end;

implementation

procedure TDisassemblerview.setDissectCodeThread(dc: TdissectCodeThread);
var i: integer;
begin
  for i:=0 to disassemblerlines.count-1 do
    TDisassemblerline(disassemblerlines[i]).dissectcode:=dc;

  fdissectCode:=dc;
  update;
end;

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
  result:=disCanvas.OnDblClick;
end;

procedure TDisassemblerview.setOnDblClick(x: TNotifyEvent);
begin
  disCanvas.OnDblClick:=x;
end;

function TDisassemblerview.getheaderWidth(headerid: integer): integer;
begin
  result:=header.Sections[headerid].width;
end;

procedure TDisassemblerview.setheaderWidth(headerid: integer; size: integer);
begin
  header.Sections[headerid].width:=size;
end;

procedure TDisassemblerview.setCommentsTab(state:boolean);
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

procedure TDisassemblerview.setTopAddress(address: dword);
begin
  fTopAddress:=address;
  fSelectedAddress:=address;
  fSelectedAddress2:=address;
  update;
end;

procedure TDisassemblerview.setSelectedAddress(address: dword);
var i: integer;
    found: boolean;
begin
  fSelectedAddress:=address;
  fSelectedAddress2:=address;

  if (fTotalvisibledisassemblerlines>0) and (InRange(fSelectedAddress, Tdisassemblerline(disassemblerlines[0]).address, Tdisassemblerline(disassemblerlines[fTotalvisibledisassemblerlines-1]).address)) then
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

procedure TDisassemblerview.wmMouseWheel (var Msg : TWMMouseWheel);
begin
  if msg.WheelDelta>0 then
  begin
    fTopAddress:=previousopcode(fTopAddress); //up
  end
  else
  begin
    disassemble(fTopAddress); //down
  end;

  update;
end;

function TDisassemblerview.getPopupMenu: Tpopupmenu;
begin
  result:=discanvas.PopupMenu;
end;

procedure TDisassemblerview.SetPopupMenu(p: tpopupmenu);
begin
  discanvas.PopupMenu:=p;
end;

procedure TDisassemblerview.HandleSpecialKey(key: word);
var i: integer;
    shiftispressed: boolean;
    ctrlispressed: boolean;
    altispressed: boolean;
begin
  beginupdate;

  shiftispressed:=GetBit(15, GetKeyState(VK_SHIFT))=1;
  ctrlispressed:=GetBit(15, GetKeyState(VK_CONTROL))=1;
  altispressed:=GetBit(15, GetKeyState(VK_MENU))=1;

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

      if (fSelectedAddress >= TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-1]).address) then
      begin
        disassemble(fTopAddress);
        update;
        fSelectedAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-1]).address;
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
var Shift: TShiftState;
    y: ^TWMKeyDown;
    x: TWMKeyDown;
begin
  y:=@msg;
  x:=y^;

  if msg.Msg=CN_KEYDOWN then
  begin
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
var i,j: integer;
    rct: trect;
    disassembled: string;
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

    if disassembleDescription.caption<>line.description then disassembleDescription.caption:=line.description;
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
  address: dword;

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
  currentAddress: dword;

  selstart, selstop: dword;
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
  selstart:=min(fSelectedAddress,fSelectedAddress2);
  selstop:=max(fSelectedAddress,fSelectedAddress2);

  while currenttop<offscreenbitmap.Height do
  begin
    if i>=disassemblerlines.Count then //add a new line
      disassemblerlines.Add(TDisassemblerLine.Create(offscreenbitmap, header.Sections, DissectCode));

    currentline:=disassemblerlines[i];

    currentline.renderLine(currentAddress,currenttop, inrange(currentAddress,selStart,selStop), currentAddress=fSelectedAddress);

    inc(currenttop, currentline.getHeight); 
    inc(i);
  end;

  fTotalvisibledisassemblerlines:=i;

  if ShowJumplines then
    renderjumplines;


  if not isupdating then
    disCanvas.Repaint;
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
  x:=(header.Sections[header.Sections.Count-1].Left+header.Sections[header.Sections.Count-1].Width);
  scrollbox.HorzScrollBar.Range:=x;
  update;
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

procedure TDisassemblerview.headerSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
begin
  updatescrollbox;
end;

procedure TDisassemblerView.OnLostFocus(sender: TObject);
begin
  self.SetFocus;
end;

constructor TDisassemblerview.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  tabstop:=true;
  DoubleBuffered:=true;

  statusinfo:=tpanel.Create(self);
  with statusinfo do
  begin
    align:=alTop;
    bevelInner:=bvLowered;
    height:=17;
    parent:=self;
  end;

  statusinfolabel:=TLabel.Create(self);
  with statusinfolabel do
  begin
    align:=alClient;
    Alignment:=taCenter;
    autosize:=false;
    transparent:=false;
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
    bevelInner:=bvNone;
    bevelOuter:=bvNone;
    Borderstyle:=bsNone;
    HorzScrollBar.Tracking:=true;
    VertScrollBar.Visible:=false;

    scrollbox.OnResize:=scrollboxResize;
    parent:=self;
    OnEnter:=OnLostFocus;
  end;



  header:=THeaderControl.Create(self);
  with header.Sections.Add do
  begin
    AllowClick:=false;
    ImageIndex:=-1;
    MinWidth:=50;
    Text:='Address';
    Width:=75;
  end;

  with header.Sections.Add do
  begin
    AllowClick:=false;
    ImageIndex:=-1;
    MinWidth:=50;
    Text:='Bytes';
    Width:=83;
  end;

  with header.Sections.Add do
  begin
    AllowClick:=false;
    ImageIndex:=-1;
    MinWidth:=50;
    Text:='Opcode';
    Width:=200;
  end;

  with header.Sections.Add do
  begin
    AllowClick:=false;
    ImageIndex:=-1;
    MinWidth:=5;
    Text:='Comment';
    Width:=97;
  end;

  with header do
  begin
    OnSectionResize:=headerSectionResize;
    parent:=scrollbox;
    onenter:=OnLostFocus;
  end;

  disCanvas:=TPaintbox.Create(self);
  with disCanvas do
  begin
    align:=alClient;
    ParentFont:=False;
    Font.Charset:=DEFAULT_CHARSET;
    Font.Color:=clwindow;
    Font.Height:=-11;
    Font.Name:='Courier';
    Font.Style:=[];
    parent:=scrollbox;
    OnPaint:=DisCanvasPaint;
    OnMouseDown:=DisCanvasMouseDown;
    OnMouseMove:=DisCanvasMouseMove;
  end;

  offscreenbitmap:=Tbitmap.Create;

  disassemblerlines:=TList.Create;

  fShowjumplineState:=jlsOnlyWithinRange;
  fShowJumplines:=true;
end;

end.

