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
     math, messages;



type TDisassemblerview=class(tpanel)
  private
    statusinfo: TPanel;
    statusinfolabel: TLabel;
    header: THeaderControl;
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
  protected
    procedure HandleSpecialKey(key: word);
    procedure WndProc(var msg: TMessage); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    procedure BeginUpdate; //stops painting until endupdate
    procedure EndUpdate;
    procedure Update;
    property Totalvisibledisassemblerlines: integer read fTotalvisibledisassemblerlines;
    constructor create(AOwner: TComponent); override;
end;

implementation

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
    if (button=mbleft) and (not (ssShift in shift)) then
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
    if disassemblerlines.Count<=i then //add a new line
      disassemblerlines.Add(TDisassemblerLine.Create(offscreenbitmap, header.Sections));

    currentline:=disassemblerlines[i];

    currentline.renderLine(currentAddress,currenttop, inrange(currentAddress,selStart,selStop), currentAddress=fSelectedAddress);

    inc(currenttop, currentline.getHeight);
    inc(i);
  end;

  fTotalvisibledisassemblerlines:=i;

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
  end;

  offscreenbitmap:=Tbitmap.Create;

  disassemblerlines:=TList.Create;


end;

end.
