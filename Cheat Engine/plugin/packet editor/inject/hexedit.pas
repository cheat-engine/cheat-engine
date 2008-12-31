unit hexedit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Math,
  Clipbrd, StdCtrls, forms;

const HE_maxByteLength=6+(16*3);
const HE_maxLineLength=HE_maxByteLength+16;
              
type
  TSLtype=(slAddress, slBytes, slCharacters);
  TOnEditByte = procedure(sender: TObject; offset: integer; oldvalue: byte; var newvalue: byte) of object;
  Thexeditor = class (TWinControl)
  private
    //graphics part
    paintbox: Tpaintbox;
    scrollbar: TScrollBar;
    hexeditimage: timage;
    charheight: integer;
    charwidth: integer;
    startline: integer; //position of the scrollbar

    //select part
    startselecttype: TSLtype;
    selecting: boolean;
    selected: boolean;
    startselect: integer;
    lastselect: integer;

    //edit part
    editing: boolean;
    FEditedOffset: integer;
    editedtype: TSLType;
    editedpart: integer; //for the hex box

    //scroll stuff
    PageSize: integer;

    //memory edit:
    buffer: pbytearray;
    buffersize: integer;
    FOnEditByte: TOnEditByte;


    procedure setupBytes;
    procedure PaintBoxPaint(Sender: TObject);
    procedure HandleSpecialKey(key: word);
    procedure CopySelectionToClipboard;

    procedure beginEdit(x,y: integer);
    procedure editoffset(offset: integer; newvalue: byte);
    function getSelectedType(x,y: integer): TSLType;
    function getSelectedOffset(x,y: integer): integer;
    procedure paintstartselect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure paintupdateselectstate(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure paintendselect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure scroll(Sender: TObject);
    procedure setEditedOffset(newvalue: integer);
    property editedoffset: integer read FEditedOffset write setEditedOffset;
    procedure wmMouseWheel (var Msg : TWMMouseWheel); message wm_MouseWheel;
  protected
    procedure Resize; override;
    procedure KeyPress(var Key: Char); override;
    procedure WndProc(var Msg: TMessage); override;

  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure setbuffer(source: pointer; size: integer);
    function getBuffer: pointer;
    procedure setSize(newSize: integer);
    procedure refresh;

    property OnEditByte: TOnEditByte read FOnEditByte write FOnEditByte;
end;

implementation

procedure THexEditor.wmMouseWheel (var Msg : TWMMouseWheel);
begin
  if msg.WheelDelta>0 then
    scrollbar.Position:=scrollbar.Position-4
  else
    scrollbar.Position:=scrollbar.Position+4;
end;

procedure THexEditor.setEditedOffset(newvalue: integer);
var linenr: integer;
    newstartline: integer;
begin
  FEditedOffset:=newvalue;
  linenr:=EditedOffset div 16;

  //sanitize
  if linenr<0 then
    linenr:=0;

  if linenr>buffersize div 16 then
    linenr:=buffersize div 16;

  newstartline:=startline;
  if newstartline>linenr then
    newstartline:=linenr;

  if linenr>newstartline+pagesize-1 then
    newstartline:=linenr-pagesize+1;

  if newstartline<>startline then
    scrollbar.Position:=newstartline;
end;

procedure THexEditor.scroll(Sender: TObject);
begin
  startline:=scrollbar.position;
  if startline>(buffersize div 16)-pagesize+1 then
  begin
    startline:=(buffersize div 16)-pagesize+1;
    if startline<0 then
      startline:=0;
    scrollbar.Position:=startline;
  end;

  refresh;
end;

procedure THexEditor.HandleSpecialKey(key: word);
var i: integer;
begin
  case key of
    VK_LEFT:
    begin
      if editedtype=slBytes then
      begin
        dec(editedpart);
        if editedpart<0 then
        begin
          editedoffset:=editedoffset-1;
          if editedoffset<0 then
          begin
            editedpart:=0;
            editedoffset:=0;
          end
          else
            editedpart:=1;
        end;
      end
      else
      begin
        //character
        editedoffset:=editedoffset-1;
        if editedoffset<0 then
          editedoffset:=0;
      end;
    end;

    VK_RIGHT:
    begin
      if editedtype=slBytes then
      begin
        inc(editedpart);
        if editedpart>1 then
        begin
          editedoffset:=editedoffset+1;
          if editedoffset>=buffersize then
          begin
            editedpart:=1;
            editedoffset:=buffersize-1;
          end
          else
            editedpart:=0;
        end;
      end
      else
      begin
        //character
        editedoffset:=editedoffset+1;
        if editedoffset>=buffersize then
          editedoffset:=buffersize-1;
      end;
    end;

    VK_UP:
    begin
      if editedoffset-16>=0 then
        editedoffset:=editedoffset-16;
    end;

    VK_DOWN:
    begin
      if editedoffset+16<buffersize then
        editedoffset:=editedoffset+16;
    end;

    VK_PRIOR:
    begin
      if editedoffset-PageSize*16>=0 then
        editedoffset:=editedoffset-PageSize*16
      else
        editedoffset:=editedoffset mod 16;
    end;

    VK_NEXT:
    begin
      if editedoffset+PageSize*16<buffersize then
        editedoffset:=editedoffset+PageSize*16
      else
      begin
        i:=editedoffset;
        while i<buffersize do
          inc(i,16);

        dec(i,16);
        editedoffset:=i;
      end;


    end;

    VK_HOME:
      editedoffset:=0;

    VK_END:
      editedoffset:=buffersize-1;

  end;

  refresh;
end; 

procedure THexeditor.WndProc(var msg: TMessage);
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

procedure Thexeditor.editoffset(offset: integer; newvalue: byte);
begin
  if assigned(FOnEditByte) then
    FOnEditByte(self, offset, buffer[offset], newvalue);

  buffer[offset]:=newvalue;
  refresh;
end;

procedure Thexeditor.CopySelectionToClipboard;
{copy selection to clipboard}
var
  beginselect, endselect: integer;
  s: string;
  i: integer;
begin
  beginselect:=min(startselect,lastselect);
  endselect:=max(startselect,lastselect);

  s:='';
  if startselecttype=slCharacters then
  begin
    for i:=beginselect to endselect do
      s:=s+chr(buffer[i]);
  end
  else
  begin
    for i:=beginselect to endselect do
      s:=s+format('%.2x ',[buffer[i]]);

    s:=copy(s,1,length(s)-1);
  end;

  Clipboard.SetTextBuf(pchar(s));
end;

procedure Thexeditor.KeyPress(var Key: Char);
var hexbyte: string;
    newbyte: integer;
    x: word;
begin
  inherited KeyPress(Key);

  if selected then
  begin
    case key of
      #3 : CopySelectionToClipboard;
    end;
  end;

  if editing then
  begin
    if editedtype=slcharacters then
    begin
      case key of
        '-': editoffset(editedoffset,buffer[editedoffset]-1);
        '+': editoffset(editedoffset,buffer[editedoffset]+1)
        else
        begin
          editoffset(editedoffset,ord(key));
          editedoffset:=editedoffset+1;
          if editedoffset>=buffersize then
            editedoffset:=buffersize-1;

          refresh;
        end;
      end;
    end
    else
    begin
      case key of
        '-':
          if editedpart=0 then
            editoffset(editedoffset,buffer[editedoffset]-16)
          else
            editoffset(editedoffset,buffer[editedoffset]-1);

        '+':
          if editedpart=0 then
            editoffset(editedoffset,buffer[editedoffset]+16)
          else
            editoffset(editedoffset,buffer[editedoffset]+1);
        else
        begin
          hexbyte:=inttohex(buffer[editedoffset],2);
          hexbyte[editedpart+1]:=key;
          if TryStrToInt('$'+hexbyte,newbyte) then
          begin
            editoffset(editedoffset,newbyte);

            inc(editedpart);
            if editedpart>=2 then
            begin
              editedoffset:=editedoffset+1;
              if editedoffset>=buffersize then
              begin
                editedoffset:=buffersize-1;
                editedpart:=1;
              end else editedpart:=0;
            end;
            refresh;
          end;
        end;
      end;




    end;
  end;
end;

function Thexeditor.getSelectedType(x,y: integer): TSLType;
{
Returns the part that is initially selected
Address, bytes, or characters
Only looking at the x
}
var
  xpos, ypos: integer;
begin
  //first 6 characters=address
  //x>=HE_maxByteLength = characters
  //anything inbetween is bytes
  xpos:=x div charwidth;
  ypos:=y div charheight+startline;

  if xpos<length(format('%.4x: ',[ypos*$10])) then
    result:=slAddress
  else
  if xpos>=HE_maxByteLength then
    result:=slCharacters
  else result:=slBytes;

end;

function Thexeditor.getSelectedOffset(x,y: integer): integer;
{
gets what part is clicked, and then uses the x and y position to pinpoint the
location accordign to charwidth and height
}
var
  xpos: integer;
  ypos: integer;
  stype: TSLType;
  addresscount: integer;
begin
  stype:=getSelectedType(x,y);
  xpos:=x div charwidth;
  ypos:=y div charheight+startline;


  addresscount:=length(format('%.4x: ',[ypos*$10]));


  case stype of
    slbytes: result:=(xpos-addresscount) div 3;
    slCharacters: result:=(xpos-HE_maxByteLength);
  end;

  if result>15 then result:=15;
  //2nd step, ypos
  result:=result+ypos*16;

  if result>=buffersize then result:=buffersize-1;
end;

procedure Thexeditor.paintstartselect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button=mbLeft then
  begin
    SetFocus;
    startselecttype:=getSelectedType(x,y);
    if startselecttype=slAddress then
    begin
      selected:=false;
      refresh;
      exit; //useless
    end;

    editing:=false;
    selected:=false;
    selecting:=true;
    startselect:=getselectedoffset(x,y);
  end;
end;

procedure Thexeditor.paintupdateselectstate(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if selecting then
  begin
    lastselect:=getselectedoffset(x,y);
    refresh;
  end;
end;

procedure Thexeditor.paintendselect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  selecting:=false;
  lastselect:=getselectedoffset(x,y);

  if lastselect=startselect then
  begin
    refresh;
    beginEdit(x,y);
  end
  else
    selected:=true;
end;

procedure Thexeditor.beginEdit(x,y: integer);
var xpos, ypos: integer;
    offset: integer;
    part: integer;
begin
  xpos:=x div charwidth;
  ypos:=y div charheight+startline;

  if ypos>(buffersize div 16) then exit;

  offset:=getSelectedOffset(x,y);

  //showmessage('beginEdit');
  if getSelectedType(x,y)=slBytes then
  begin
    //find which part of the 2 byte hex
    part:=(xpos-length(format('%.4x: ',[ypos*$10]))) mod 3;
    if part=2 then
    begin
      part:=1;
      xpos:=xpos-1;
    end;

    editing:=true;
    editedoffset:=offset;
    editedtype:=slBytes;
    editedpart:=part;
  end
  else
  begin
    editing:=true;
    editedoffset:=offset;
    editedtype:=slCharacters;
  end;

  refresh;
end;

procedure Thexeditor.refresh;
var i,j,k: integer;
    s, charstring: string;
    linecount: integer;
    xpos: integer;
    highlight: boolean;
    minselect,maxselect: integer;
    currentbyte: integer;
    maxbyte: integer;
begin
  linecount:=(buffersize-(startline*16)+15) div 16;
  if linecount>pagesize+1 then linecount:=pagesize+1;

  hexeditimage.Canvas.FillRect(rect(0,0,hexeditimage.Width,hexeditimage.Height));

  highlight:=selecting or selected;
  if highlight then
  begin
    minselect:=min(startselect,lastselect);
    maxselect:=max(startselect,lastselect);
  end;

  maxbyte:=startline*16+linecount*16;
  for i:=0 to linecount-1 do
  begin    
    xpos:=0;
    s:=format('%.4x:', [(i+startline)*16]);
    charstring:='';

    hexeditimage.Canvas.TextOut(xpos*charwidth,i*charheight,s);
    inc(xpos,length(s));


    currentbyte:=(i+startline)*16;
    j:=0;
    while (currentbyte<buffersize) and (j<16) do
    begin
      s:=format(' %.2x',[buffer[(i+startline)*16+j]] );

      if highlight and (currentbyte>=minselect) and (currentbyte<=maxselect) then
      begin
        hexeditimage.Canvas.Font.Color:=clRed;
      end
      else
      begin
        hexeditimage.Canvas.Font.Color:=clWindowText;
      end;

      if editing and (currentbyte=editedoffset) then
      begin
        if editedtype = slBytes then
        begin
          for k:=1 to length(s) do
          begin
            if editedpart=k-2 then
            begin
              hexeditimage.Canvas.Brush.Color:=clBlack;
              hexeditimage.Canvas.Font.Color:=clWhite;
            end;
            hexeditimage.Canvas.TextOut((xpos+k-1)*charwidth,i*charheight,s[k]);

            hexeditimage.Canvas.Font.Color:=clWindowText;
            hexeditimage.Canvas.Brush.Color:=clGray;
          end;
        end
        else
        begin
          hexeditimage.Canvas.Brush.Color:=clGray;
          hexeditimage.Canvas.TextOut((xpos+1)*charwidth,i*charheight,s[2]+s[3]);
          hexeditimage.Canvas.Brush.Color:=clBlack; //for the next char, which is an edit
          hexeditimage.Canvas.Font.Color:=clWhite;
        end;
        inc(xpos,length(s));

        if buffer[(i+startline)*16+j]<32 then s:='.' else s:=char(buffer[(i+startline)*16+j]);

        hexeditimage.Canvas.TextOut((HE_maxByteLength+j)*charwidth,i*charheight,s);
                
      end
      else
      begin
        hexeditimage.Canvas.TextOut(xpos*charwidth,i*charheight,s);
        inc(xpos,length(s));

        if buffer[(i+startline)*16+j]<32 then s:='.' else s:=char(buffer[(i+startline)*16+j]);

        hexeditimage.Canvas.TextOut((HE_maxByteLength+j)*charwidth,i*charheight,s);
      end;

      hexeditimage.Canvas.Font.Color:=clWindowText;
      hexeditimage.Canvas.Brush.Color:=clWindow;



      inc(j);
      inc(currentbyte);
    end;
  end;


  paintbox.repaint;
end;

procedure Thexeditor.setupBytes;
begin

  if (buffersize div 16)<scrollbar.PageSize then
    scrollbar.Max:=scrollbar.PageSize
  else
    scrollbar.Max:=(buffersize div 16);
  refresh;
end;

procedure THexEditor.setSize(newSize: integer);
var newbuf: pointer;
begin
  if newSize>0 then
  begin
    getmem(newbuf,newSize);
    if buffer<>nil then
    begin
      CopyMemory(newbuf,buffer,min(buffersize,newSize));
      freemem(buffer);
    end;
    
    buffer:=newbuf;
    buffersize:=newSize;
  end else
  begin
    if buffer<>nil then
    begin
      freemem(buffer);
      buffersize:=0;
      buffer:=nil;
    end;
  end;

  setupBytes;
  Resize;
end;

function THexEditor.getBuffer: pointer;
begin
  result:=buffer;
end;

procedure THexEditor.setBuffer(source: pointer; size: integer);
{Copies the buffer to the class's internal buffer}
begin
  selecting:=false;
  selected:=false;
  
  if buffer<>nil then
  begin
    freemem(buffer);
    buffersize:=0;
  end;

  getmem(buffer,size);
  try
    CopyMemory(buffer,source,size);
  except
    raise exception.Create('Failure copying the buffer');
  end;

  try
    buffersize:=size;

    setupBytes;
    Resize;
  except
    raise exception.Create('Internal error');
  end;
end;

procedure THexEditor.PaintBoxPaint(Sender: TObject);
var cr: Trect;
begin
  cr:=PaintBox.Canvas.ClipRect;
  PaintBox.Canvas.CopyRect(cr,hexeditimage.Canvas,cr);
end;

procedure THexEditor.Resize;
var m: integer;
begin
  inherited Resize;
  //check to see if we should show the scrollbar at all:

  if charHeight>0 then
  begin
    PageSize:=paintbox.height div charHeight;
    if pagesize*16>buffersize then
    begin
      if scrollbar.Visible then
        scrollbar.Visible:=false;
      scrollbar.Position:=0;
    end
    else
    begin
      if not scrollbar.Visible then
        scrollbar.Visible:=true;
    end;
  end;

  hexeditimage.free;
  hexeditimage:=TImage.Create(self);

  hexeditimage.Width:=paintbox.Width;
  hexeditimage.Height:=paintbox.height;
  hexeditimage.Canvas.Font.Name:='Courier';
  hexeditimage.Canvas.Font.Size:=12;
  hexeditimage.Canvas.Brush.Color:=clWindow;
  hexeditimage.Canvas.Pen.Color:=clBlack;
  hexeditimage.Canvas.FillRect(rect(0,0,paintbox.width,paintbox.height));

  charHeight:=hexeditimage.Canvas.TextHeight('X');
  charWidth:=hexeditimage.Canvas.TextWidth('X');


  scrollbar.PageSize:=PageSize;
  scrollbar.LargeChange:=PageSize-1;


  refresh;
end;

constructor Thexeditor.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  color:=clWindow;
  {HorzScrollBar.Tracking:=true;
  VertScrollBar.Tracking:=true; }

  scrollbar:=Tscrollbar.Create(self);
  scrollbar.Kind:=sbVertical;
  scrollbar.Align:=alRight;
  scrollbar.OnChange:=Scroll;
  scrollbar.Parent:=self;

  paintbox:=Tpaintbox.Create(self);
  paintbox.Align:=alClient;
  hexeditimage:=TImage.Create(self);

  paintbox.OnPaint:=paintboxpaint;
  paintbox.OnMouseDown:=paintstartselect;
  paintbox.OnMouseMove:=paintupdateselectstate;
  paintbox.OnMouseUp:=paintendselect;
  paintbox.Parent:=self;
  DoubleBuffered:=true;
  Resize;

  enabled:=true;
  tabstop:=true;
  Refresh;
end;

destructor Thexeditor.destroy;
begin
  freeandnil(paintbox);
  freeandnil(hexeditimage);
  inherited destroy;
end;

end.
