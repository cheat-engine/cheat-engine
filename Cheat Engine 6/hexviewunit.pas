unit hexviewunit; 

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, forms, controls, StdCtrls, ExtCtrls, comctrls, graphics,
  lmessages, menus,commctrl, symbolhandler, cefuncproc, newkernelhandler, math;

type
  THexRegion=(hrInvalid, hrByte, hrChar);
  TDisplayType = (dtByte, dtWord, dtDword, dtDwordDec, dtSingle, dtDouble);

  TPageinfo=record
    readable: boolean;
    inModule: boolean;
  end;

  THexView=class(TPanel)
  private
    buffer: pbytearray;
    buffersize: integer;
    pageinfo: array of TPageinfo;

    verticalscrollbar: TScrollbar;
    mbCanvas: TPaintbox;
    offscreenBitmap: TBitmap;
    fAddress: ptrUint;

    textheight: integer;
    addresswidthdefault: integer;
    charsize, bytesize, byteSizeWithoutChar: integer;
   // _8bytelinesize: integer;

    memoryInfo: string;

    addresswidth: integer;
    usablewidth: integer;
    bytesPerLine: integer;
    totallines: integer;
    charstart: integer;
    bytestart: integer;

    editing: boolean;


    isSelecting, hasSelection: boolean;
    selected, selected2: ptrUint;

    isEditing: boolean;
    editingCursorPos: integer;
    editingType: THexRegion;
    fDisplayType: TDisplayType; //determines what to display. If anything other than byte the editing/selecting mode will be disabled


    lastupdate: dword;

    procedure LoadMemoryRegion;
    procedure UpdateMemoryInfo;
    procedure OnLostFocus(sender: TObject);
    procedure mbPaint(sender: TObject);
    procedure hexviewResize(sender: TObject);
    procedure setAddress(a: ptrUint);
    procedure render;
    function getByte(a: ptrUint; var unreadable: boolean): byte; overload;
    function getByte(a: ptrUint): string; overload;
    function getWord(a: ptrUint): string;
    function getDWord(a: ptrUint): string;
    function getDWordDec(a: ptrUint): string;
    function getSingle(a: ptrUint): string;
    function getDouble(a: ptrUint): string;
    function getChar(a: ptrUint): char;
    function inModule(a: ptrUint): boolean;
    procedure MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure mbCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mbCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure mbCanvasMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mbCanvasDoubleClick(Sender: TObject);
    function getAddressFromPosition(x, y: integer; var region: THexRegion): ptrUint;
    procedure RefocusIfNeeded;
    procedure HandleEditKeyPress(key: char);
    procedure setDisplayType(newdt: TDisplaytype);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  public
    procedure update;
    procedure changeSelected;
    procedure AddSelectedAddressToCheatTable;
    function getAddressFromCurrentMousePosition(var region: THexRegion): ptrUint;
    property address: ptrUint read fAddress write setAddress;
    property DisplayType: TDisplayType read fDisplayType write setDisplayType;
    constructor create(AOwner: TComponent); override;
  end;

implementation

uses formsettingsunit, Valuechange, AddAddress;

procedure THexView.setDisplayType(newdt: TDisplaytype);
begin
  fDisplayType:=newdt;

  if fDisplayType<>dtByte then
  begin
    isSelecting:=false;
    hasSelection:=false;
    isEditing:=false;
  end;
  update;
end;

procedure THexView.HandleEditKeyPress(key: char);
var b: byte;
    unreadable: boolean;
    bw: dword;
    x: byte;
begin
  if not isediting then exit;

  b:=getByte(selected,unreadable);
  if unreadable then exit; //unreadable

  if (editingtype=hrByte) then
  begin
    if (not (key in ['a'..'f','A'..'F','0'..'9'])) then exit; //hex edit and not a hexadecimal value

    x:=strtoint('$'+key);

    if editingCursorPos=0 then
    begin
      x:=x shl (4);
      b:=b and $0F;
      b:=b or x;
    end
    else
    begin
      b:=b and $F0;
      b:=b or x;
    end;

    WriteProcessMemory(processhandle, pointer(selected), @b, 1, bw);

    inc(editingCursorPos);
    if editingCursorPos>=2 then
    begin
      selected:=selected+1;
      editingCursorPos:=0;
    end;
  end else
  begin
    b:=ord(key);
    WriteProcessMemory(processhandle, pointer(selected), @b, 1, bw);
    selected:=selected+1;
  end;


  update;
end;

procedure THexView.RefocusIfNeeded;
var lastaddress: ptrUint;
beforeoffset: ptrUint;
afterOffset: ptrUint;
column: integer;
begin
  if isEditing then
  begin
    //check if the selected address in in the visible section, if not, adjust
    lastaddress:=fAddress+bytesperline*(totallines-2);
    if not inrangex(selected, faddress, lastaddress) then
    begin
      //outside, find out if it's above or below

      column:=(selected - fAddress) mod bytesperline;

      beforeOffset:=fAddress-selected;
      afterOffset:=selected-lastaddress;
      if beforeOffset>afteroffset then
        faddress:=fAddress+afterOffset-column
      else
        faddress:=fAddress-beforeOffset-column;

      update;
    end;
  end;
end;

function THexView.getAddressFromCurrentMousePosition(var region: THexRegion): ptrUint;
var
  r: trect;
  cursorpos: TPoint;
  x,y: integer;
begin
  GetWindowRect(Handle, r);

  X:=mouse.CursorPos.X-r.Left;
  Y:=mouse.CursorPos.Y-r.Top;

  result:=getAddressFromPosition(x,y,region);
end;

function THexView.getAddressFromPosition(x, y: integer; var region: THexRegion): ptrUint;
var row: integer;
    column: integer;
    byteclickpos: integer;
begin
  region:=hrInvalid;
  result:=0;

  //find what part is selected
  if y>textheight*2 then
  begin
    row:=(y-textheight*2) div textheight;

    if InRange(x,bytestart,bytestart+bytesperline*byteSizeWithoutChar-charsize) then
    begin
      //byteclick
      //which byte ?
      column:=(x-bytestart) div byteSizeWithoutChar;

      //check if it's a click on the space
      byteclickpos:=x-(bytestart+column*byteSizeWithoutChar);
      if byteclickpos>(2.5*charsize) then
      begin
        result:=getAddressFromPosition(x+charsize,y,region); //let it get handled by the next one if there is one
        exit;
      end;

      region:=hrByte;
      result:=fAddress+bytesperline*row+column;
    end
    else
    if InRange(x,charstart,charstart+bytesperline*charsize) then
    begin
      //charclick
      column:=(x-charstart) div charsize;

      region:=hrChar;
      result:=fAddress+bytesperline*row+column;
    end;
  end; //else it's a headerclick

end;

procedure THexView.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);

  HandleEditKeyPress(key);
end;

procedure THexView.KeyDown(var Key: Word; Shift: TShiftState);
var b: byte;
x: dword;
begin

  if shift=[] then
  begin
    case key of
      VK_ESCAPE:
      begin
        isEditing:=false;
        update;
      end;

      vk_up:
      begin
        if isEditing then
          dec(selected,bytesPerLine)
        else
          dec(faddress,bytesPerLine);

        update;
      end;

      vk_down:
      begin
        if isEditing then
          inc(selected,bytesPerLine)
        else
          inc(faddress,bytesPerLine);
        update;
      end;

      vk_left:
      begin
        if isEditing then
        begin
          if editingType=hrChar then
            dec(selected)
          else
          begin
            dec(editingCursorPos);
            if editingCursorPos<0 then
            begin
              selected:=selected-1;
              editingCursorPos:=1;
            end;
          end;
        end
        else
          dec(faddress);
        update;
      end;

      vk_right:
      begin
        if isEditing then
        begin
          if editingType=hrChar then
            inc(selected)
          else
          begin
            inc(editingCursorPos);
            if editingCursorPos>=2 then
            begin
              selected:=selected+1;
              editingCursorPos:=0;
            end;
          end;

        end
        else
          inc(faddress);
        update;
      end;

      vk_prior:
      begin
        if isEditing then
          dec(selected,bytesPerLine*(totallines-1))
        else
          dec(faddress,bytesPerLine*(totallines-1));
        update;
      end;

      vk_next:
      begin
        if isEditing then
          inc(selected,bytesPerLine*(totallines-1))
        else
          inc(faddress,bytesPerLine*(totallines-1));
        update;
      end;

      VK_ADD,VK_SUBTRACT:
      begin
        if isEditing then
        begin
          if ReadProcessMemory(processhandle, pointer(selected),@b,1,x) then
          begin
            if key=VK_SUBTRACT then
              dec(b)
            else
              inc(b);

            WriteProcessMemory(processhandle, pointer(selected),@b,1,x);
            update;
          end;
        end;
      end;


    end;

  end
  else
  begin
    if (ssCtrl in shift) and (not (ssAlt in shift)) then
    begin
      case key of
        VK_1: DisplayType:=dtByte;
        VK_2: DisplayType:=dtWord;
        VK_3: DisplayType:=dtDword;
        VK_4: DisplayType:=dtDwordDec;
        VK_5: DisplayType:=dtSingle;
        VK_6: DisplayType:=dtDouble;
      end;
    end;

  end;
  RefocusIfNeeded;

  {
          if key in [96..105] then ///numpad fix
          key:=key-96+ord('0');
          }
  inherited KeyDown(key,shift);
end;


procedure THexView.AddSelectedAddressToCheatTable;
begin
  if hasSelection then
  begin
    //selected
    if addform=nil then
      addform:=Taddform.create(self);

    addform.NewAddress.text:=inttohex(selected,8);
    addform.showmodal;
  end;
end;

procedure THexView.ChangeSelected;
var unreadable: boolean;
begin
  if hasSelection then
  begin
    getByte(selected,unreadable);
    if unreadable then exit;

    with Tvaluechangeform.Create(application) do
    begin
      address:=selected;
      ShowModal;
    end;
    update;
  end;
end;

procedure THexView.mbCanvasDoubleClick(Sender: TObject);
begin
  changeSelected;
end;

procedure THexView.mbCanvasMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var hr: THexRegion;
a: ptrUint;
byteclickpos: integer;
begin


  if button=mbleft then
  begin
    if isSelecting then
    begin
      //let's emulate one more mosemove just to be sure
      mbCanvasMouseMove(sender, [ssLeft], x,y);

      //selected2 is now properly updated
      isSelecting:=false;

      if selected=selected2 then
      begin
        a:=getAddressFromPosition(x,y,hr);
        if (a<>selected) or (hr=hrInvalid) then exit; //out of bounds exit

        isEditing:=true;
        hasSelection:=false;



        editingType:=hr;
      end;
    end;
  end;
  update;
end;

procedure THexView.mbCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  address: ptrUint;
  hr: THexRegion;
begin
  if isSelecting and (ssLeft in shift) then
  begin
    address:=getAddressFromPosition(x,y,hr);

    if hr<>hrInvalid then
      selected2:=address;

    update;
  end;
end;

procedure THexView.mbCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hr: THexRegion;
  oldselected: ptrUint;
  byteclickpos: integer;
begin
  setfocus;



  if button=mbLeft then
  begin
    hasSelection:=false;
    oldselected:=selected;
    selected:=getAddressFromPosition(x,y,hr);
    selected2:=selected;

    if hr<>hrInvalid then
    begin
      hasSelection:=fDisplayType=dtByte;
      isSelecting:=fDisplayType=dtByte; //only start selecting if the type is byte

      if isEditing then
      begin
        if oldselected<>selected then
        begin
          isEditing:=false;
          editingCursorPos:=0;
        end
        else
        begin
          if hr=hrByte then
          begin
            //update the cursor position
            byteclickpos:=(x-bytestart)-(((x-bytestart) div byteSizeWithoutChar) * bytesizeWithoutChar);
            editingCursorPos:=(byteclickpos div charsize);
            if editingCursorPos>1 then
              editingCursorPos:=1;

          end;
        end;
      end;



    end else isEditing:=false;
  end;

  update;
end;

procedure THexView.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var delta: integer;
begin
  SetFocus; //get the focus back
  case scrollcode of
    scLineUp:   dec(faddress,bytesPerLine);
    scLineDown: inc(faddress,bytesPerLine);
    scPageDown: inc(faddress,bytesPerLine*(totallines-1));
    scPageUp:   dec(faddress,bytesPerLine*(totallines-1));
    sctrack:
    begin
      delta:=scrollpos-50;
      faddress:=faddress+bytesPerLine*delta;
    end;
  end;

  update;
  scrollpos:=50;
end;

procedure THexView.MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var i: integer;
begin
  if Focused then i:=2 else i:=4;
  if WheelDelta>0 then
    fAddress:=fAddress-(bytesPerLine*i)
  else
    fAddress:=fAddress+(bytesPerLine*i);

  update;
end;

procedure THexView.UpdateMemoryInfo;
var
  mbi: TMEMORYBASICINFORMATION;
  a: ptrUint;
  a64: qword;
  mi: TModuleInfo;
begin

  zeromemory(@mbi,sizeof(mbi));
  Virtualqueryex(processhandle,pointer(fAddress),mbi,sizeof(mbi));
  memoryInfo:='Protect:';

  if (mbi.AllocationProtect and PAGE_NOACCESS)>0 then memoryInfo:=memoryInfo+'No Access ';
  if (mbi.AllocationProtect and PAGE_READONLY)>0 then memoryInfo:=memoryInfo+'Read Only ';
  if (mbi.AllocationProtect and PAGE_READWRITE)>0 then memoryInfo:=memoryInfo+'Read/Write ';
  if (mbi.AllocationProtect and PAGE_WRITECOPY)>0 then memoryInfo:=memoryInfo+'Write Copy ';
  if (mbi.AllocationProtect and PAGE_EXECUTE)>0 then memoryInfo:=memoryInfo+'Execute ';
  if (mbi.AllocationProtect and PAGE_EXECUTE_READ)>0 then memoryInfo:=memoryInfo+'Execute/Read only ';
  if (mbi.AllocationProtect and PAGE_EXECUTE_READWRITE)>0 then memoryInfo:=memoryInfo+'Execute/Read/Write ';
  if (mbi.AllocationProtect and PAGE_EXECUTE_WRITECOPY)>0 then memoryInfo:=memoryInfo+'Execute/Write Copy ';
  if (mbi.AllocationProtect and PAGE_GUARD)>0 then memoryInfo:=memoryInfo+'Guarded ';
  if (mbi.AllocationProtect and PAGE_NOCACHE)>0 then memoryInfo:=memoryInfo+'Not Cached';


  memoryInfo:=memoryInfo+' Base='+IntToHex(ptrUint(mbi.BaseAddress),8)+' Size='+IntTohex(mbi.RegionSize,1);

  if (formsettings<>nil) and formsettings.cbKernelOpenProcess.checked and assigned(GetPhysicalAddress) and GetPhysicalAddress(processhandle,pointer(fAddress),a64) then
    memoryInfo:=memoryInfo+' Physical Address='+IntToHex(a64,8);


  if symhandler.getmodulebyaddress(fAddress,mi) then
    memoryInfo:=memoryInfo+' Module='+mi.modulename;
end;

procedure THexView.LoadMemoryRegion;
var memorysize: integer;
    pages: integer;
    startpage: ptrUint;
    endpage: ptrUint;
    currentaddress: ptrUint;
    i: integer;
    bufferpos: ptrUint;
    blocksize: integer;
    actualread: dword;
begin
  memorysize:=bytesPerLine*totallines;
  if buffersize<memorysize then //make sure the bufferi s big enough
  begin
    ReAllocMem(buffer,memorysize*2);
    buffersize:=memorysize*2;
  end;

  startpage:=fAddress;
  endpage:=fAddress+memorysize;

  startpage:=startpage shr 12;
  endpage:=endpage shr 12;

  pages:=endpage-startpage+1;
  setlength(pageinfo,pages);

  currentaddress:=fAddress;

  bufferpos:=ptrUint(buffer);
  for i:=0 to pages-1 do
  begin
    blocksize:=4096-(currentaddress and $fff);
    blocksize:=min(memorysize,blocksize);

    pageinfo[i].readable:=readprocessmemory(processhandle,pointer(currentaddress),pointer(bufferpos), blocksize, actualread);
    pageinfo[i].inModule:=symhandler.inModule(currentaddress);

    inc(bufferpos,blocksize);
    inc(currentaddress,blocksize);

    dec(memorysize,blocksize);
  end;

end;

function THexView.inModule(a: ptrUint): boolean;
var i: integer;
    page: word;
begin
  result:=false;
  i:=a-fAddress;
  if i>buffersize then
    exit;

  page:=((fAddress and $fff)+i) shr 12;
  result:=(page<length(pageinfo)) and (pageinfo[page].readable) and (pageinfo[page].inModule);

end;

function THexView.getByte(a: ptrUint; var unreadable: boolean): byte; overload;
var i: integer;
    page: word;
begin
  unreadable:=true;
  result:=0;

  i:=a-fAddress;
  if i>buffersize then
    exit;

  page:=((fAddress and $fff)+i) shr 12;


  if page<length(pageinfo) then
  begin
    unreadable:=not pageinfo[page].readable;
    if not unreadable then
      result:=buffer[i];
  end;// else messagebox(0,pchar('fAddress='+inttohex(fAddress,8)+' a='+inttohex(a,8)+' i='+inttohex(i,2)+' page='+inttohex(page,8)+' page2='+inttohex(page2,8)+' length(readable)='+inttostr(length(readable))),'aaa',0);
end;

function THexView.getByte(a: ptrUint): string; overload;
var err: boolean;
    b: byte;
begin
  b:=getbyte(a,err);
  if err then
    result:='??'
  else
    result:=inttohex(b,2);

end;

function THexView.getWord(a: ptrUint): string;
var
  w: word;
  pw: pbytearray;
  err,err2: boolean;
begin
  pw:=@w;
  pw[0]:=getbyte(a,err);
  pw[1]:=getbyte(a+1,err2);

  if err or err2 then
    result:='????'
  else
    result:=inttohex(w,4);
end;

function THexView.getDWord(a: ptrUint): string;
var
  dw: dword;
  pdw: pbytearray;
  err,err2,err3,err4: boolean;
begin
  pdw:=@dw;
  pdw[0]:=getbyte(a,err);
  pdw[1]:=getbyte(a+1,err2);
  pdw[2]:=getbyte(a+2,err3);
  pdw[3]:=getbyte(a+3,err4);

  if err or err2 or err3 or err4 then
    result:='????????'
  else
    result:=inttohex(dw,8);
end;

function THexView.getDWordDec(a: ptrUint): string;
var
  dw: dword;
  pdw: pbytearray;
  err,err2,err3,err4: boolean;
begin
  pdw:=@dw;
  pdw[0]:=getbyte(a,err);
  pdw[1]:=getbyte(a+1,err2);
  pdw[2]:=getbyte(a+2,err3);
  pdw[3]:=getbyte(a+3,err4);

  if err or err2 or err3 or err4 then
    result:='???'
  else
    result:=inttostr(dw);

  if length(result)>11 then
    result:=copy(result,1,8)+'...';
end;

function THexView.getSingle(a: ptrUint): string;
var
  s: single;
  ps: pbytearray;
  err,err2,err3,err4: boolean;
begin
  ps:=@s;
  ps[0]:=getbyte(a,err);
  ps[1]:=getbyte(a+1,err2);
  ps[2]:=getbyte(a+2,err3);
  ps[3]:=getbyte(a+3,err4);

  if err or err2 or err3 or err4 then
    result:='???'
  else
    result:=format('%f',[s]);

  if length(result)>11 then
    result:=copy(result,1,8)+'...';
end;

function THexView.getDouble(a: ptrUint): string;
var
  d: single;
  pd: pbytearray;
  err,err2,err3,err4,err5,err6,err7,err8: boolean;
begin
  pd:=@d;
  pd[0]:=getbyte(a,err);
  pd[1]:=getbyte(a+1,err2);
  pd[2]:=getbyte(a+2,err3);
  pd[3]:=getbyte(a+3,err4);
  pd[4]:=getbyte(a+4,err5);
  pd[5]:=getbyte(a+5,err6);
  pd[6]:=getbyte(a+6,err7);
  pd[7]:=getbyte(a+7,err8);


  if err or err2 or err3 or err4 or err5 or err6 or err7 or err8 then
    result:='???'
  else
    result:=format('%f',[d]);

  if length(result)>20 then
    result:=copy(result,1,18)+'...';
end;


function THexView.getChar(a: ptrUint): char;
var err: boolean;
    b: byte;
begin
  b:=getbyte(a,err);
  if err then
    result:='?'
  else
  begin
    if b in [0..31] then
      result:='.'
    else
      result:=chr(b);
  end;
end;


procedure THexView.render;
var
  currentaddress: ptrUint;

  i,j: integer;
  x: word;
  cheader, bheader: string;
  bytepos: integer;
  initialoffset: byte;
  seperators: array of integer;
  seperatorindex: integer;
begin
  if Parent=nil then exit;

  setlength(seperators, bytesperline shr 3);
  seperatorindex:=0;

  currentaddress:=fAddress;

  offscreenbitmap.Canvas.TextOut(0,0,memoryInfo);
  offscreenbitmap.Canvas.TextOut(0,textheight,'address');

  bheader:='';
  cheader:='';

  initialoffset:=currentaddress and $f;
  for i:=0 to bytesperline-1 do
  begin
    case displayType of
      dtByte: bheader:=bHeader+inttohex(initialoffset+i,2)+' ';
      dtWord: if (i mod 2)=0 then bheader:=bHeader+inttohex(initialoffset+i,2)+' ' else bheader:=bHeader+'   ';
      dtDWord,dtDwordDec,dtSingle: if (i mod 4)=0 then bheader:=bHeader+inttohex(initialoffset+i,2)+' ' else bheader:=bHeader+'   ';
      dtDouble: if (i mod 8)=0 then bheader:=bHeader+inttohex(initialoffset+i,2)+' ' else bheader:=bHeader+'   ';
    end;

    cheader:=cheader+inttohex((initialoffset+i) and $f,1);

    if (((initialoffset+i) and 7)=7) and (i<>bytesperline-1) then
    begin
      seperators[seperatorindex]:=i;
      inc(seperatorindex);
    end;
  end;

//  bheader:='bpl='+inttostr(bytesperline)+' bytesize='+inttostr(bytesize)+' width='+inttostr(mbcanvas.width);
  offscreenbitmap.Canvas.TextOut(bytestart,textheight, bheader);
  offscreenbitmap.Canvas.TextOut(charstart,textheight, cheader);



  for i:=0 to totallines-1 do
  begin
    offscreenbitmap.Canvas.TextOut(0, (2+i)*textheight,inttohex(currentaddress,8));

    bytepos:=0;
    for j:=0 to bytesperline-1 do
    begin
      if inModule(currentaddress) then
        offscreenbitmap.canvas.Font.Color:=clGreen
      else
        offscreenbitmap.canvas.Font.Color:=clWindowText;

      if hasSelection and inrangex(currentaddress,minx(selected,selected2),maxx(selected,selected2)) then
        offscreenbitmap.canvas.Font.Color:=clRed;


      if isEditing and (currentAddress=selected) then
      begin
        if (editingtype=hrByte) then
        begin
          offscreenbitmap.canvas.Brush.Color:=clHighlight;
          offscreenbitmap.canvas.Font.Color:=clHighlightText;
        end
        else
        begin
          offscreenbitmap.canvas.Brush.Color:=clYellow;
          offscreenbitmap.canvas.Font.Color:=clWindowText;
        end;
      end;

      case displayType of
        dtByte: offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, (2+i)*textheight, getByte(currentAddress)); //byte
        dtWord: if (j mod 2)=0 then offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, (2+i)*textheight, getWord(currentAddress));
        dtDWord: if (j mod 4)=0 then offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, (2+i)*textheight, getDWord(currentAddress));
        dtDWordDec: if (j mod 4)=0 then offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, (2+i)*textheight, getDWordDec(currentAddress));
        dtSingle: if (j mod 4)=0 then offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, (2+i)*textheight, getSingle(currentAddress));
        dtDouble: if (j mod 8)=0 then offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, (2+i)*textheight, getDouble(currentAddress));
      end;


      if isEditing and (currentAddress=selected) then
      begin
        if (editingtype=hrChar) then
        begin
          offscreenbitmap.canvas.Brush.Color:=clHighlight;
          offscreenbitmap.canvas.Font.Color:=clHighlightText;
        end
        else
        begin
          offscreenbitmap.canvas.Brush.Color:=clYellow;
          offscreenbitmap.canvas.Font.Color:=clWindowText;
        end;
      end;
      offscreenbitmap.canvas.TextOut(charstart+j*charsize, (2+i)*textheight, getChar(currentAddress)); //char


      offscreenbitmap.canvas.Font.Color:=clWindowText;
      offscreenbitmap.canvas.Brush.Color:=clBtnFace;

      if isEditing and (currentAddress=selected) then
      begin
        //render the carret
        offscreenbitmap.canvas.Pen.Width:=2;
        offscreenbitmap.canvas.Pen.Color:=clRed;
        if editingtype=hrByte then //draw the carret for the byte
          offscreenbitmap.Canvas.Line(1+bytestart+bytepos*charsize+editingCursorPos*charsize,(2+i)*textheight+1,1+bytestart+bytepos*charsize+editingCursorPos*charsize,(3+i)*textheight-2)
        else //draw the carret for the char
          offscreenbitmap.Canvas.Line(1+charstart+j*charsize,(2+i)*textheight+1,1+charstart+j*charsize,(3+i)*textheight-2);

        offscreenbitmap.canvas.Pen.Width:=1;

      end;

      bytepos:=bytepos+3;
      inc(currentaddress);
    end;
  end;



  for i:=0 to seperatorindex-1 do
  begin
    offscreenbitmap.Canvas.Pen.Color:=clYellow;
    offscreenbitmap.Canvas.PenPos:=point(bytestart+(seperators[i]+1)*byteSizeWithoutChar-(charsize shr 1),textheight);
    offscreenbitmap.Canvas.LineTo(bytestart+(seperators[i]+1)*byteSizeWithoutChar-(charsize shr 1),mbcanvas.height);

    offscreenbitmap.Canvas.PenPos:=point(charstart+(seperators[i]+1)*charsize,textheight);
    offscreenbitmap.Canvas.LineTo(charstart+(seperators[i]+1)*charsize,mbcanvas.height);
  end;

  offscreenbitmap.Canvas.Pen.Color:=clBlack;
  offscreenbitmap.Canvas.PenPos:=point(0,textheight*2);
  offscreenbitmap.Canvas.LineTo(charstart+bytesperline*charsize,textheight*2);
end;

procedure THexView.setAddress(a: ptrUint);
begin
  fAddress:=a;
  update;
end;

procedure THexView.hexviewResize(sender: TObject);
var oldsizex,oldsizey: integer;
    seperatorcount: integer;
begin
  {$ifdef cpu64}
  if fAddress<ptrUint($100000000) then
    addresswidth:=addresswidthdefault
  else
    addresswidth:=offscreenbitmap.Canvas.TextWidth(inttohex(fAddress,8));
  {$else}
  addresswidth:=addresswidthdefault;
  {$endif}

  oldsizex:=bytesperline;
  oldsizey:=totallines;

  bytestart:=addresswidth+8;

  usablewidth:=mbCanvas.ClientWidth-addresswidth-8;

  bytesPerLine:=(usablewidth div bytesize) and $fffffff8;
  if bytesperline=0 then
    bytesperline:=8;

  charstart:=bytestart+bytesperline*byteSizeWithoutChar;


  totallines:=1+(mbCanvas.clientHeight-(textheight*2)) div textheight;  //-(textheight*2) for the header


  if (oldsizex<>bytesperline) or (oldsizey<>totallines) then
    update;
end;

procedure THexView.update;
var oldAddressWidth: integer;
{$ifdef cpu64}
defaultrange: ptrUint;
{$endif}
begin
  //if (gettickcount-lastupdate)>50 then
  begin
    if offscreenbitmap.Width<mbcanvas.width then
      offscreenbitmap.Width:=mbcanvas.width;

    if offscreenbitmap.Height<mbCanvas.width then
      offscreenbitmap.Height:=mbcanvas.Height;

    offscreenbitmap.Canvas.Brush.Color:=clBtnFace;
    offscreenbitmap.Canvas.FillRect(mbcanvas.ClientRect);

    oldAddressWidth:=addresswidth;
    {$ifdef cpu64}
    defaultrange:=$100000000;
    if fAddress<defaultrange then
      addresswidth:=addresswidthdefault
    else
      addresswidth:=offscreenbitmap.Canvas.TextWidth(inttohex(fAddress,8));
    {$else}
    addresswidth:=addresswidthdefault;
    {$endif}

    if oldAddressWidth<>addresswidth then
      hexviewResize(self);

    LoadMemoryRegion;
    updateMemoryInfo;
    render;

    mbcanvas.Repaint;
    lastupdate:=gettickcount;
  end;
end;

procedure THexView.OnLostFocus(sender: TObject);
begin
  self.SetFocus;
end;

procedure THexView.mbPaint(sender: TObject);
var cr: Trect;
begin
  cr:=mbcanvas.Canvas.ClipRect;
  mbcanvas.Canvas.CopyRect(cr,offscreenbitmap.Canvas,cr);
end;

constructor THexView.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  getmem(buffer,8192);
  buffersize:=8192;

  width:=200;
  height:=200;

  verticalscrollbar:=TScrollBar.Create(self);
  with verticalscrollbar do
  begin
    align:=alright;
    kind:=sbVertical;
    pagesize:=2;
    position:=50;

    parent:=self;
    OnEnter:=OnLostFocus;
   // OnChange:=scrollbarChange;
   // OnKeyDown:=scrollbarKeyDown;
    OnScroll:=scrollBarScroll;
  end;

  mbCanvas:=TPaintbox.Create(self);
  with mbCanvas do
  begin
    align:=alClient;
    ParentFont:=False;
    Font.Charset:=DEFAULT_CHARSET;
    Font.Color:=clwindowText;
    Font.Height:=-11;
    Font.Name:='Courier';
    Font.Style:=[];
    parent:=self;
    OnPaint:=MbPaint;
    OnMouseDown:=mbCanvasMouseDown;
    OnMouseMove:=mbCanvasMouseMove;
    OnMouseUp:=mbCanvasMouseUp;
    OnMouseWheel:=mousescroll;
    OnDblClick:=mbCanvasDoubleClick;
  end;

  self.OnResize:=hexviewResize;

  offscreenbitmap:=TBitmap.create;
  offscreenbitmap.canvas.font.Assign(mbCanvas.font);

  textheight:=offscreenbitmap.Canvas.TextHeight('X?');
  addresswidthdefault:=offscreenbitmap.Canvas.TextWidth('XXXXXXXX');

  charsize:=offscreenbitmap.Canvas.TextWidth('X');
  byteSize:=offscreenbitmap.Canvas.TextWidth('XX X'); //byte space and the character it represents
  byteSizeWithoutChar:=offscreenbitmap.Canvas.TextWidth('XX ');


  update;

end;

end.

