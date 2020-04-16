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

uses {$ifdef darwin}macport,messages,lcltype,{$endif}
     {$ifdef windows}jwawindows, windows,commctrl,{$endif}
     sysutils, LCLIntf, forms, classes, controls, comctrls, stdctrls, extctrls, symbolhandler,
     cefuncproc, NewKernelHandler, graphics, disassemblerviewlinesunit, disassembler,
     math, lmessages, menus, DissectCodeThread

     {$ifdef USELAZFREETYPE}
     ,cefreetype,FPCanvas, EasyLazFreeType, LazFreeTypeFontCollection, LazFreeTypeIntfDrawer,
     LazFreeTypeFPImageDrawer, IntfGraphics, fpimage, graphtype
     {$endif}
     ;



type TShowjumplineState=(jlsAll, jlsOnlyWithinRange);     

type TDisassemblerSelectionChangeEvent=procedure (sender: TObject; address, address2: ptruint) of object;
type TDisassemblerExtraLineRender=function(sender: TObject; Address: ptruint; AboveInstruction: boolean; selected: boolean; var x: integer; var y: integer): TRasterImage of object;
type TDisassemblerViewOverrideCallback=procedure(address: ptruint; var addressstring: string; var bytestring: string; var opcodestring: string; var parameterstring: string; var specialstring: string) of object;


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

    lastselected1, lastselected2: ptruint; //reminder for the update on what the previous address was

    fSelectedAddress: ptrUint; //normal selected address
    fSelectedAddress2: ptrUint; //secondary selected address (when using shift selecting)
    fTopAddress: ptrUint; //address to start disassembling from

    fTopSubline: integer; //in case of multiline lines

    fShowJumplines: boolean; //defines if it should draw jumplines or not
    fShowjumplineState: TShowjumplineState;


   // fdissectCode: TDissectCodeThread;

    lastupdate: dword;
    destroyed: boolean;

    fOnSelectionChange: TDisassemblerSelectionChangeEvent;
    fOnExtraLineRender: TDisassemblerExtraLineRender;

    fspaceAboveLines: integer;
    fspaceBelowLines: integer;
    fjlThickness: integer;
    fjlSpacing: integer;

    fhidefocusrect: boolean;

    scrolltimer: ttimer;

    fOnDisassemblerViewOverride: TDisassemblerViewOverrideCallback;



    procedure updateScrollbox;
    procedure scrollboxResize(Sender: TObject);

    //-scrollbar-
    procedure scrollUp(sender: TObject);
    procedure scrollDown(sender: TObject);
    procedure updateScroller(speed: integer);
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
    procedure SetOriginalPopupMenu(p: Tpopupmenu);
    function getOriginalPopupMenu: Tpopupmenu;
    procedure setSelectedAddress(address: ptrUint);
    procedure setSelectedAddress2(address: ptrUint);
    procedure setTopAddress(address: ptrUint);
    function getOnDblClick: TNotifyEvent;
    procedure setOnDblClick(x: TNotifyEvent);
    procedure renderJumpLines;
    procedure setJumpLines(state: boolean);
    procedure setJumplineState(state: tshowjumplinestate);
    procedure synchronizeDisassembler;
    procedure StatusInfoLabelCopy(sender: TObject);


  protected
    procedure HandleSpecialKey(key: word);
    procedure WndProc(var msg: TMessage); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoAutoSize; override;

  published
    property OnKeyDown;
    property OnDblClick: TNotifyEvent read getOnDblClick write setOnDblClick;

  public
    colors: TDisassemblerViewColors;
    jlCallColor: TColor;
    jlConditionalJumpColor: TColor;
    jlUnConditionalJumpColor: TColor;

    LastFormActiveEvent: qword;

    {$ifdef USELAZFREETYPE}
    FTFont: TFreeTypeFont;
    FTFontb: TFreeTypeFont; //bold version
    IntfImage: TLazIntfImage;
    drawer: TIntfFreeTypeDrawer;
    {$endif}

    procedure DoDisassemblerViewLineOverride(address: ptruint; var addressstring: string; var bytestring: string; var opcodestring: string; var parameterstring: string; var specialstring: string);

    procedure reinitialize; //deletes the assemblerlines

    procedure BeginUpdate; //stops painting until endupdate
    procedure EndUpdate;
    procedure Update; override;
    procedure setCommentsTab(state: boolean);

    function getheaderWidth(headerid: integer): integer;
    procedure setheaderWidth(headerid: integer; size: integer);

    property Totalvisibledisassemblerlines: integer read fTotalvisibledisassemblerlines;

    procedure getDefaultColors(var c: Tdisassemblerviewcolors);

    function getDisassemblerLineAtPoint(p: tpoint): TDisassemblerLine;
    function getReferencedByLineAtPos(p: tpoint): ptruint;
    function ClientToCanvas(p: tpoint): TPoint;

    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
  published
    property HideFocusRect: boolean read fhidefocusrect write fhidefocusrect;
    property SpaceAboveLines: integer read fspaceAboveLines write fspaceAboveLines;
    property SpaceBelowLines: integer read fspaceBelowLines write fspaceBelowLines;
    property jlThickness: integer read fjlThickness write fjlThickness;
    property jlSpacing: integer read fjlSpacing write fjlSpacing;
    property ShowJumplines: boolean read fShowJumplines write setJumpLines;
    property ShowJumplineState: TShowJumplineState read fShowjumplinestate write setJumplineState;
    property TopAddress: ptrUint read fTopAddress write setTopAddress;
    property SelectedAddress: ptrUint read fSelectedAddress write setSelectedAddress;
    property SelectedAddress2: ptrUint read fSelectedAddress2 write setSelectedAddress2;
    property OnSelectionChange: TDisassemblerSelectionChangeEvent read fOnSelectionChange write fOnSelectionChange;
    property PopupMenu: TPopupMenu read getOriginalPopupMenu write SetOriginalPopupMenu;
    property Osb: TBitmap read offscreenbitmap;
    property OnExtraLineRender: TDisassemblerExtraLineRender read fOnExtraLineRender write fOnExtraLineRender;
    property OnDisassemblerViewOverride: TDisassemblerViewOverrideCallback read fOnDisassemblerViewOverride write fOnDisassemblerViewOverride;
end;


implementation

uses processhandlerunit, parsers, Clipbrd, Globals;

resourcestring
  rsSymbolsAreBeingLoaded = 'Symbols are being loaded (%d %%)';
  rsStructuresAreBeingParsed = 'Structures are being parsed';
  rsExtendedDebugInfoIsLoaded = 'Extended debug info is being loaded (%d %%)';
  rsPleaseOpenAProcessFirst = 'Please open a process first';
  rsAddress = 'Address';
  rsBytes = 'Bytes';
  rsOpcode = 'Opcode';
  rsComment = 'Comment';
  rsCopy = 'Copy';

procedure TDisassemblerview.DoDisassemblerViewLineOverride(address: ptruint; var addressstring: string; var bytestring: string; var opcodestring: string; var parameterstring: string; var specialstring: string);
var i: integer;
begin
  if assigned(fOnDisassemblerViewOverride) then
    fOnDisassemblerViewOverride(address, addressstring, bytestring, opcodestring, parameterstring, specialstring);
end;

procedure TDisassemblerview.SetOriginalPopupMenu(p: Tpopupmenu);
begin
  inherited popupmenu:=p;
end;

function TDisassemblerview.getOriginalPopupMenu: Tpopupmenu;
begin
  result:=inherited popupmenu;
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
  if discanvas<>nil then
    result:=disCanvas.OnDblClick
  else
    result:=nil;
end;

procedure TDisassemblerview.setOnDblClick(x: TNotifyEvent);
begin
  if discanvas<>nil then
    disCanvas.OnDblClick:=x;
end;

function TDisassemblerview.getheaderWidth(headerid: integer): integer;
begin
  if header<>nil then
    result:=header.Sections[headerid].width
  else
    result:=0;
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
    begin
      fTopAddress:=address;
      fTopSubline:=0;
    end;

  end else
  begin
    fTopAddress:=address;
    fTopSubline:=0;
  end;



  update;
end;

procedure TDisassemblerview.MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var pos: integer;
begin
//  messagebox(0,'scroll','',0);
  pos:=0;
  if wheelDelta>0 then
    scrollBarScroll(sender, scLineUp, pos)
  else
    scrollBarScroll(sender, scLineDown, pos)

//  update;

end;

         {
function TDisassemblerview.getPopupMenu: Tpopupmenu;
begin
  if discanvas<>nil then
    result:=discanvas.PopupMenu
  else
    result:=TControl(self).PopupMenu;
end;

procedure TDisassemblerview.SetPopupMenu(p: tpopupmenu);
begin
  TControl(self).PopupMenu:=p;
  if discanvas<>nil then
    discanvas.PopupMenu:=p;
end;        }

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

  {if msg.msg=WM_MOUSEWHEEL then
  begin
//    messagebox(0,'wm_mousewheel','',0);

  end;}
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

procedure TDisassemblerview.DoAutoSize;
begin
  DisableAutoSizing;
  disassembleDescription.ClientHeight:=disassembleDescription.Canvas.TextHeight('GgXxYj')+4;
  header.Height:=Canvas.TextHeight('GgXxyJjlL')+4;

  disassembleDescription.Font.Height:=GetFontData(font.Handle).Height;

  EnableAutoSizing;
  inherited DoAutoSize;
end;

procedure TDisassemblerview.DisCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var timepassed: qword;
begin
  timepassed:=gettickcount64-LastFormActiveEvent;
  if timepassed>=50 then
  begin
    if ssLeft in shift then
      DisCanvas.OnMouseDown(self,mbleft,shift,x,y);
  end
  else
  begin
    //outputdebugstring('skipped due to '+inttostr(timepassed)+' milliseconds');
  end;
end;

procedure TDisassemblerview.DisCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
    found: boolean;
    line: TDisassemblerLine;
begin
  if not Focused then SetFocus;


  if (y<0) or (y>discanvas.Height) then exit; //moved out of range

  //find the selected line
  line:=nil;
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
  if (line=nil) or (not found) then exit; //not found, which is weird, but whatever...



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
  trianglesize: integer;
  jumplineoffset: integer;
begin
  if fTotalvisibledisassemblerlines=0 then exit;

  trianglesize:=TDisassemblerLine(disassemblerlines[0]).defaultHeight div 4;
  jumplineoffset:=trianglesize;

  found:=false;

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

      inc(jumplineoffset,jlSpacing);
    end;
  end;
end;

procedure TDisassemblerview.synchronizeDisassembler;
begin
  visibleDisassembler.showmodules:=symhandler.showModules;
  visibleDisassembler.showsymbols:=symhandler.showsymbols;
end;

procedure TDisassemblerview.StatusInfoLabelCopy(sender: TObject);
begin
  Clipboard.AsText:=statusinfolabel.Caption;
end;

function TDisassemblerview.ClientToCanvas(p: tpoint): TPoint;
begin

  result:=p;
  dec(result.y, disCanvas.top+scrollbox.top);
end;

function TDisassemblerview.getReferencedByLineAtPos(p: tpoint): ptruint;
var cp: tpoint;
  d: TDisassemblerLine;
  y: integer;
begin
  result:=0;
  cp:=ClientToCanvas(p);
  d:=getDisassemblerLineAtPoint(p);
  if d<>nil then
    result:=d.getReferencedByAddress(cp.y-d.top);
end;

function TDisassemblerview.getDisassemblerLineAtPoint(p: tpoint): TDisassemblerLine;
var cp: tpoint; //canvas point
  i: integer;
begin
  //checks the y coordinate and returns the appropriate disassemblerline
  //p is in disassemblerview coordinates, convert it to line coordinates

  result:=nil;
  cp:=ClientToCanvas(p);

  if cp.y>=0 then
  begin
    for i:=0 to disassemblerlines.Count-1 do
      if InRange(cp.y, TDisassemblerLine(disassemblerlines[i]).top, TDisassemblerLine(disassemblerlines[i]).top+TDisassemblerLine(disassemblerlines[i]).height) then
      begin
        result:=TDisassemblerLine(disassemblerlines[i]);
        exit;
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
  currentAddress: ptrUint;

  selstart, selstop: ptrUint;
  description: string;
  x: ptrUint;

  {$ifdef USELAZFREETYPE}
  b: tbitmap;
  {$endif}
begin
  inherited update;

  if destroyed then exit;

  synchronizeDisassembler;

  //if gettickcount-lastupdate>50 then
  begin
    if (symhandler.loadingExtendedData or symhandler.parsingStructures or (not symhandler.isloaded)) and (not symhandler.haserror) then
    begin
      if processid>0 then
      begin
       // symhandler.currentState:=
        if symhandler.loadingExtendedData then
          statusinfolabel.Caption:=format(rsExtendedDebugInfoIsLoaded,[symhandler.extendedDataProgess])
        else
        if symhandler.parsingStructures then
          statusinfolabel.Caption:=rsStructuresAreBeingParsed
        else
          statusinfolabel.Caption:=format(rsSymbolsAreBeingLoaded,[symhandler.progress])
      end
      else
        statusinfolabel.Caption:=rsPleaseOpenAProcessFirst;

    end
    else
    begin
      if symhandler.haserror then
        statusinfolabel.Font.Color:=clRed
      else
        statusinfolabel.Font.Color:=clWindowText;

      statusinfolabel.Caption:=AnsiToUtf8(symhandler.getnamefromaddress(TopAddress,symhandler.showsymbols, symhandler.showmodules,nil,nil,8,false));
    end;

    //initialize bitmap dimensions
    if discanvas.width>scrollbox.HorzScrollBar.Range then
      offscreenbitmap.Width:=discanvas.width
    else
      offscreenbitmap.Width:=scrollbox.HorzScrollBar.Range;
    offscreenbitmap.Height:=discanvas.Height;

    //clear bitmap
    {$ifdef USELAZFREETYPE}
    if (not UseOriginalRenderingSystem) and (drawer<>nil) then
    begin
      if (IntfImage.Width<>offscreenbitmap.width) or (IntfImage.Height<>offscreenbitmap.Height) then
        IntfImage.SetSize(offscreenbitmap.width, offscreenbitmap.height);

      drawer.FillPixels(TColorToFPColor(ColorToRGB(clBtnFace)));
    end
    else
    {$endif}
    begin
      offscreenbitmap.Canvas.Brush.Color:=clBtnFace;
      offscreenbitmap.Canvas.FillRect(rect(0,0,offscreenbitmap.Width, offscreenbitmap.Height));
    end;

    currenttop:=-fTopSubline;
    i:=0;

    currentAddress:=fTopAddress;
    selstart:=minX(fSelectedAddress,fSelectedAddress2);
    selstop:=maxX(fSelectedAddress,fSelectedAddress2);

    offscreenbitmap.Canvas.Font:=font;

    while currenttop<offscreenbitmap.Height do
    begin
      while i>=disassemblerlines.Count do //add a new line
        disassemblerlines.Add(TDisassemblerLine.Create(self, offscreenbitmap, header.Sections, @colors));

      currentline:=disassemblerlines[i];


      currentline.renderLine(currentAddress,currenttop, inrangeX(currentAddress,selStart,selStop), currentAddress=fSelectedAddress, fspaceAboveLines, fSpaceBelowLines);

      inc(currenttop, currentline.getHeight);
      inc(i);
    end;

    {$ifdef USELAZFREETYPE}
    if (not UseOriginalRenderingSystem) and (IntfImage<>nil) then
    begin
      b:=tbitmap.create();
      b.LoadFromIntfImage(IntfImage);
      offscreenbitmap.Canvas.Draw(0,0,b);
      b.free;
    end;
    {$endif}


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


  if assigned(fOnSelectionChange) then
  begin
    //check if there has been a change since last update
    if (lastselected1<>fSelectedAddress) or (lastselected2<>fselectedaddress2) then
      fOnSelectionChange(self, fselectedaddress, fselectedaddress2);
  end;

  lastselected1:=fselectedaddress;
  lastselected2:=fSelectedAddress2;



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

  discanvas.height:=scrollbox.ClientHeight-header.height;

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

procedure TDisassemblerView.scrollUp(sender: tobject);
var
  i: integer;
  pos: integer;

begin
  beginupdate;

  pos:=verticalscrollbar.Position;
  for i:=0 to floor(power(abs(pos-50),0.5)) do
  begin
    scrollBarScroll(nil,scLineUp, pos);
    update;
  end;

  EndUpdate;
end;

procedure TDisassemblerView.scrollDown(sender: TObject);
var
  i: integer;
  pos: integer;

begin
  beginupdate;

  pos:=verticalscrollbar.Position;
  for i:=0 to floor(power(abs(pos-50),0.5)) do
  begin
    scrollBarScroll(nil,scLineDown, pos);
    update;
  end;

  EndUpdate;
end;

procedure TDisassemblerView.updateScroller(speed: integer);
begin
  if (speed<>0) then
  begin
    if scrolltimer=nil then
      scrolltimer:=ttimer.create(self);

    //max speed is 50 (50 and -50)
    scrolltimer.Interval:=10+100-(abs(speed)*(100 div 50));

    if speed<0 then
      scrolltimer.OnTimer:=scrollUp
    else
      scrolltimer.OnTimer:=scrollDown;

    scrolltimer.enabled:=true;
  end
  else
  begin
    if scrolltimer<>nil then
      scrolltimer.enabled:=false;

  end;

end;

procedure TDisassemblerView.scrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var x: integer;
    found: boolean;
    temp: string;
    delta: integer;
    i: integer;

    dl: TDisassemblerLine;
    shiftispressed: boolean;
begin
  if sender<>nil then
    beginupdate;
  
  if scrollcode=sctrack then
  begin
    delta:=scrollpos-50;
    updatescroller(delta);
     {
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
    }
    endupdate;
    exit;
  end;

  shiftispressed:=GetBit(15, GetKeyState(VK_SHIFT))=1;

  case scrollcode of
    scLineUp:
    begin
      if shiftispressed then
        fTopAddress:=fTopAddress-1
      else
      begin
        dl:=TDisassemblerLine(disassemblerlines[0]);

        dec(fTopSubline, dl.defaultHeight);
        if fTopSubline<0 then
        begin
          fTopAddress:=previousopcode(fTopAddress);

          update; //this will generate the proper disassemblerline data but won't render as beginupdate was called (if called from the scroll updater beginupdate was called there)

          dl:=TDisassemblerLine(disassemblerlines[0]);
          inc(fTopSubline, dl.height);
          if fTopSubline<0 then
            fTopSubline:=0;

        end;
      end;
    end;

    scLineDown:
    begin
      found:=false;

      if shiftispressed then
        fTopAddress:=fTopAddress+1
      else
      if fTotalvisibledisassemblerlines>0 then
      begin
        dl:=TDisassemblerLine(disassemblerlines[0]);
        inc(fTopSubline, dl.defaultHeight); //go to the next line

        //if the current position is bigger than the line, go to the next address (most of the time)
        if fTopSubline>=dl.height then //next address
        begin
          for x:=0 to fTotalvisibledisassemblerlines-1 do
            if fTopAddress<>TDisassemblerLine(disassemblerlines[x]).address then
            begin
              fTopAddress:=TDisassemblerLine(disassemblerlines[x]).address;
              found:=true;
              break;
            end;

          if not found then //disassemble
            disassemble(fTopAddress,temp);

          fTopSubline:=fTopSubline mod dl.defaultHeight;
        end;
      end;
    end;

    scPageDown:
    begin
      if TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-1]).address=fTopAddress then
      begin
        for x:=0 to fTotalvisibledisassemblerlines-1 do
          disassemble(fTopAddress,temp);
      end
      else fTopAddress:=TDisassemblerLine(disassemblerlines[fTotalvisibledisassemblerlines-1]).address;

      fTopSubline:=0;
    end;

    scPageUp:
    begin
      for x:=0 to fTotalvisibledisassemblerlines-2 do
        fTopAddress:=previousopcode(fTopAddress);  //go 'numberofaddresses'-1 times up

      fTopSubline:=0;
    end;

    scEndScroll:
    begin
      scrollpos:=50;
      updatescroller(0);
    end;


  end;

  if sender<>nil then //not sent from the component
  begin
    scrollpos:=50;      //i dont want the slider to work 100%

    endupdate;
    SetFocus;
  end;
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
var
  emptymenu: TPopupMenu;
  mi: TMenuItem;
begin
  inherited create(AOwner);

  {$ifdef USELAZFREETYPE}
  if loadCEFreeTypeFonts then
  begin
    FTFont:=TFreeTypeFont.Create;
    FTFont.Name:='Courier New';


    FTFontb:=TFreeTypeFont.Create;
    FTFontb.Name:='Courier New';
    FTFontb.Style:=[ftsBold];
    FTFontb.Hinted:=false;

    IntfImage:=TLazIntfImage.Create(0,0, [riqfRGB]);
    drawer:=TIntfFreeTypeDrawer.Create(IntfImage);
  end;
  {$endif}

  jlSpacing:=2;
  jlThickness:=1;

  emptymenu:=TPopupMenu.create(self);

  tabstop:=true;
  DoubleBuffered:=true;
  AutoSize:=false;

  statusinfo:=tpanel.Create(self);
  with statusinfo do
  begin
    autosize:=true;
    ParentFont:=true;
    align:=alTop;
    bevelInner:=bvLowered;
//    height:=19;
    parent:=self;
    PopupMenu:=emptymenu;
   // color:=clYellow;
  end;

  statusinfolabel:=TLabel.Create(self);
  with statusinfolabel do
  begin
    parentfont:=true;
    align:=alClient;
    Alignment:=taCenter;
    autosize:=true;
    //font.Size:=25;
    //transparent:=false;
    parent:=statusinfo;
    PopupMenu:=TPopupMenu.Create(statusinfolabel);
    with popupmenu do
    begin
      name:='StatusInfoLabelPopupMenu';
      mi:=tmenuitem.create(PopupMenu);
      mi.caption:=rsCopy;
      mi.OnClick:=StatusInfoLabelCopy;
      mi.name:='miStatusInfoLabelCopy';
      items.Add(mi);
    end;
  end;

  disassembleDescription:=Tpanel.Create(self);
  with disassembleDescription do
  begin
    align:=alBottom;
    //autosize:=true;
    bevelInner:=bvLowered;
    bevelOuter:=bvLowered;
    Color:=clWhite;

    ParentFont:=false;
    Font.Charset:=DEFAULT_CHARSET;
    Font.Color:=clBtnText;
    Font.Name:='Courier New';
    Font.Style:=[];

    parent:=self;
    PopupMenu:=emptymenu;


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
    //PopupMenu:=emptymenu;
    //OnMouseWheel:=MouseScroll;
  end;


  header:=THeaderControl.Create(self);



  with header do
  begin
    top:=0;
    //autosize:=true;
    //height:=20;
    OnSectionResize:=headerSectionResize;
    OnSectionTrack:=headerSectionTrack;
    parent:=scrollbox;
    onenter:=OnLostFocus;
    //header.Align:=alTop;
    //header.ParentFont:=false;
    PopupMenu:=emptymenu;

    name:='Header';
  end;



  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=50;
    Text:=rsAddress;
    Width:=80;
   end;

  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=50;
    Text:=rsBytes;
    Width:=140;
  end;

  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=50;
    Text:=rsOpcode;
    Width:=200;
  end;

  with header.Sections.Add do
  begin
    ImageIndex:=-1;
    MinWidth:=5;
    Text:=rsComment;
    AutoSize:=true;

    Width:=100;

  end;



  disCanvas:=TPaintbox.Create(self);
  with disCanvas do
  begin
    AnchorSideTop.control:=header;
    anchorsidetop.Side:=asrBottom;
    ParentFont:=true; //False;

    AnchorSideBottom.control:=scrollbox;
    AnchorSideBottom.side:=asrBottom;

    anchors:=[akBottom, akLeft, akTop, akRight];


    parent:=scrollbox;
    OnPaint:=DisCanvasPaint;
    OnMouseDown:=DisCanvasMouseDown;
    OnMouseMove:=DisCanvasMouseMove;
    OnMouseWheel:=mousescroll;
    name:='PaintBox';
  end;

  offscreenbitmap:=Tbitmap.Create;


  disassemblerlines:=TList.Create;

  fShowjumplineState:=jlsOnlyWithinRange;
  fShowJumplines:=true;

  self.OnMouseWheel:=mousescroll;




  getDefaultColors(colors);


  scrollbox.AutoSize:=false;
  scrollbox.AutoScroll:=false;

end;

procedure TDisassemblerview.getDefaultColors(var c: Tdisassemblerviewcolors);
begin
  //setup the default colors:
  c[csNormal].backgroundcolor:=clBtnFace;
  c[csNormal].normalcolor:=clWindowText;
  c[csNormal].registercolor:=clRed;
  c[csNormal].symbolcolor:=clGreen;
  c[csNormal].hexcolor:=clBlue;

  c[csHighlighted].backgroundcolor:=clHighlight;
  c[csHighlighted].normalcolor:=clHighlightText;
  c[csHighlighted].registercolor:=clRed;
  c[csHighlighted].symbolcolor:=clLime;
  c[csHighlighted].hexcolor:=clYellow;

  c[csSecondaryHighlighted].backgroundcolor:=clGradientActiveCaption;
  c[csSecondaryHighlighted].normalcolor:=clHighlightText;
  c[csSecondaryHighlighted].registercolor:=clRed;
  c[csSecondaryHighlighted].symbolcolor:=clBlue;
  c[csSecondaryHighlighted].hexcolor:=clOlive;

  c[csBreakpoint].backgroundcolor:=clRed;
  c[csBreakpoint].normalcolor:=clBlack;
  c[csBreakpoint].registercolor:=clGreen;
  c[csBreakpoint].symbolcolor:=clLime;
  c[csBreakpoint].hexcolor:=clBlue;

  c[csHighlightedbreakpoint].backgroundcolor:=clGreen;
  c[csHighlightedbreakpoint].normalcolor:=clWhite;
  c[csHighlightedbreakpoint].registercolor:=clRed;
  c[csHighlightedbreakpoint].symbolcolor:=clLime;
  c[csHighlightedbreakpoint].hexcolor:=clBlue;

  c[csSecondaryHighlightedbreakpoint].backgroundcolor:=clGreen;
  c[csSecondaryHighlightedbreakpoint].normalcolor:=clWhite;
  c[csSecondaryHighlightedbreakpoint].registercolor:=clRed;
  c[csSecondaryHighlightedbreakpoint].symbolcolor:=clLime;
  c[csSecondaryHighlightedbreakpoint].hexcolor:=clBlue;

  //ultimap2
  c[csUltimap].backgroundcolor:=clYellow;
  c[csUltimap].normalcolor:=clBlack;
  c[csUltimap].registercolor:=clGreen;
  c[csUltimap].symbolcolor:=clBlue;
  c[csUltimap].hexcolor:=clBlue;

  c[csHighlightedUltimap].backgroundcolor:=clGreen;
  c[csHighlightedUltimap].normalcolor:=clWhite;
  c[csHighlightedUltimap].registercolor:=clRed;
  c[csHighlightedUltimap].symbolcolor:=clLime;
  c[csHighlightedUltimap].hexcolor:=clBlue;

  c[csSecondaryHighlightedUltimap].backgroundcolor:=clGreen;
  c[csSecondaryHighlightedUltimap].normalcolor:=clWhite;
  c[csSecondaryHighlightedUltimap].registercolor:=clRed;
  c[csSecondaryHighlightedUltimap].symbolcolor:=clLime;
  c[csSecondaryHighlightedUltimap].hexcolor:=clBlue;

  jlConditionalJumpColor:=clRed;
  jlUnconditionalJumpColor:=clGreen;
  jlCallColor:=clYellow;
end;


end.

