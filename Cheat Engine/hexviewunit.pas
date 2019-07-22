unit hexviewunit; 

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, forms, controls, StdCtrls, ExtCtrls, comctrls, graphics,
  lmessages, menus,commctrl, symbolhandler, symbolhandlerstructs, cefuncproc, newkernelhandler, math,
  Clipbrd,dialogs, changelist, DebugHelper, debuggertypedefinitions, maps, contnrs,
  strutils, byteinterpreter, commonTypeDefs, lazutf8, lazutf16, lcltype;

type
  TDisplayType = (dtByte, dtByteDec, dtWord, dtWordDec, dtDword, dtDwordDec, dtQword, dtQwordDec, dtSingle, dtDouble);
  TCharEncoding = (ceAscii, ceCodepage, ceUtf8, ceUtf16);

const
  DisplayTypeByteSize: array [dtByte..dtDouble] of integer =(1,1, 2,2, 4, 4, 8,8, 4, 8); //update both if adding something new


type
  TByteSelectEvent=procedure(sender: TObject; address: ptruint; address2: ptruint) of object;
  TAddressChangeEvent=procedure(sender: TObject; address: ptruint) of object;
  THexViewTextRenderEvent=procedure(sender: TObject; address: ptruint; var text: string) of object;

  THexRegion=(hrInvalid, hrByte, hrChar);
  TPageinfo=record
    baseaddress: ptruint;
    readable: boolean;
    inModule: boolean;
    data: array [0..4095] of byte;
  end;

  PPageinfo=^TPageInfo;



  THexView=class(TCustomPanel)
  private
    MemoryMap: TMap;
    MemoryMapItterator: TMapIterator;

    verticalscrollbar: TScrollbar;
    mbCanvas: TPaintbox;
    offscreenBitmap: TBitmap;
    fAddress: ptrUint;

    textheight: integer;
    addresswidthdefault: integer;
    charsize, bytesize, byteSizeWithoutChar: integer;

    memoryInfo: string;
    memoryInfo_allocationbasepos: integer;
    memoryInfo_allocationbaseend: integer;
    memoryinfo_baseaddresspos: integer;
    memoryinfo_baseaddressend: integer;

    addresswidth: integer;
    usablewidth: integer;
    bytesPerLine: integer;
    fbytesPerSeperator: integer; //only 8, 4 or 2
    flockedRowSize: integer; //if 0 then bytesPerLine is calculated by the size of the object, else it's lockedRowSize

    totallines: integer;
    charstart: integer;
    bytestart: integer;

    editing: boolean;


    isSelecting, fhasSelection: boolean;
    selected, selected2: ptrUint;

    isEditing: boolean;
    editingCursorPos: integer;
    editingType: THexRegion;
    selectionType: THexRegion;
    fDisplayType: TDisplayType; //determines what to display. If anything other than byte the editing/selecting mode will be disabled
    fCharEncoding: TCharEncoding;


    lastupdate: dword;
    changelist: TChangelist;

    //lock on vars:
    fLockedTo: THexview;
    fLockedToBaseAddress: ptruint;

    fShowDiffHv: THexview;

    backlist: TStack;

    fOnByteSelect: TByteSelectEvent;
    fonAddressChange: TAddressChangeEvent;

    fOnCharacterRender: THexViewTextRenderEvent;
    fOnValueRender: THexViewTextRenderEvent;

    lastaddress: ptruint;
    lastselection1, lastselection2: ptruint;

    scrolltimer: TTimer;


    fHexFont: Tfont;
    fspaceBetweenLines: integer;

    fuseRelativeBase: boolean;
    fRelativeBase: ptruint;

    usedRelativeBase: boolean;


    procedure setHexFont(f: TFont);

    procedure LoadMemoryRegion;
    function GetPageInfo(a: ptruint): PPageInfo;

    procedure UpdateMemoryInfo;
    procedure OnLostFocus(sender: TObject);
    procedure mbPaint(sender: TObject);
    procedure hexviewResize(sender: TObject);
    procedure setAddress(a: ptrUint);
    procedure render;
    procedure setByte(a: ptrUint;value: byte);

    function getUTF8CharByteLength(a: ptruint): integer;
    function getUTF16CharByteLength(a: ptruint): integer;

    function getByte(a: ptrUint; var unreadable: boolean): byte; overload;
    function getByte(a: ptrUint): string; overload;
    function getWord(a: ptrUint): string;
    function getDWord(a: ptrUint): string;
    function getDwordValue(a: ptruint; out unreadable: boolean): dword;
    function getQWordValue(a: ptruint; out unreadable: boolean): qword;
    function getQWord(a: ptrUint): string;
    function getByteDec(a: ptrUint; full: boolean=false): string;
    function getWordDec(a: ptrUint; full: boolean=false): string;
    function getDWordDec(a: ptrUint; full: boolean=false): string;
    function getQWordDec(a: ptrUint; full: boolean=false): string;
    function getSingle(a: ptrUint; full: boolean=false): string;
    function getDouble(a: ptrUint; full: boolean=false): string;
    function getChar(a: ptrUint; out charlength: integer): string;
    function inModule(a: ptrUint): boolean;
    procedure MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure mbCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mbCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure mbCanvasMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mbCanvasDoubleClick(Sender: TObject);
    function getAddressFromPosition(x, y: integer; var region: THexRegion): ptrUint;
    procedure RefocusIfNeeded;
    procedure HandleEditKeyPress(wkey: tutf8char);
    procedure setDisplayType(newdt: TDisplaytype);
    procedure setCharEncoding(newce: TCharEncoding);

    function CalculateGradientColor(Percentage: single; MaxColor, MinColor: TColor): TColor;
    procedure setBytesPerSeperator(b: integer);
    function gethasSelection: boolean;

    function getSelectionStart: ptruint;
    function getSelectionStop: ptruint;
    procedure updateScroller(speed: integer);

    procedure lineUp(sender: tobject);
    procedure lineDown(sender: TObject);

  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    fadetimer: integer;
    statusbar: TStatusbar;
    procedure LockRowsize(size: integer=0);
    procedure UnlockRowsize;
    procedure CopySelectionToClipboard;
    procedure GetSelectionRange(var start: ptruint; var stop: ptruint);
    procedure PasteFromClipboard;

    {$warn 3057 off}
    procedure update; //hidden on purpose, really, no override here
    procedure changeSelected;
    procedure AddSelectedAddressToCheatTable;
    function getAddressFromCurrentMousePosition(var region: THexRegion): ptrUint;

    procedure ShowDifference(hv: THexview);
    procedure EndDifferenceView;

    procedure Lock(hv: THexview);
    procedure Unlock;
    function isLocked: boolean;
    function isShowingDifference: boolean;
    function hasBackList: boolean;
    procedure Back;
    function CanFollow: boolean;
    procedure Follow;

    procedure AddToBackList(address: pointer);


    constructor create(AOwner: TComponent); override;
    destructor destroy; override;



    property history: TStack read backlist;
  published
    property Address: ptrUint read fAddress write setAddress;
    property TopAddress: ptrUint read fAddress write setAddress;
    property HasSelection: boolean read gethasSelection;
    property SelectionStart: ptruint read getSelectionStart;
    property SelectionStop: ptruint read getSelectionStop;
    property Osb: TBitmap read offscreenbitmap;
    property DisplayType: TDisplayType read fDisplayType write setDisplayType;
    property CharEncoding: TCharEncoding read fCharEncoding write setCharEncoding;
    property BytesPerSeperator: integer read fbytesPerSeperator write setBytesPerSeperator;
    property OnByteSelect: TByteSelectEvent read fOnByteSelect write fOnByteSelect;
    property OnAddressChange: TAddressChangeEvent read fonAddressChange write fonAddressChange;
    property OnCharacterRender: THexViewTextRenderEvent read fOnCharacterRender write fOnCharacterRender;
    property OnValueRender: THexViewTextRenderEvent read fOnValueRender write fOnValueRender;


    property PaintBox: TPaintbox read mbCanvas;
    property OSBitmap: TBitmap read offscreenBitmap;
    property HexFont: TFont read fHexFont write setHexFont;
    property LockedRowSize: integer read fLockedRowSize write fLockedRowSize;
    property spaceBetweenLines: integer read fspaceBetweenLines write fspaceBetweenLines;
    property UseRelativeBase: boolean read fUseRelativeBase write fUseRelativeBase;
    property RelativeBase: ptruint read fRelativeBase write fRelativeBase;
  end;

implementation

uses formsettingsunit, Valuechange, MainUnit, ProcessHandlerUnit, parsers,
  StructuresFrm2, MemoryBrowserFormUnit, BreakpointTypeDef;

resourcestring
  rsBigFuckingError = 'Big fucking error';
  rsInvalidBytesPerSeperatorValue = 'Invalid BytesPerSeperator value:%s';
  rsThisLooksLikeAnArrayOfByteDoYouWantToInputItAsAHex = 'This looks like an '
    +'array of byte. Do you want to input it as a hexadecimal string?';
  rsThisLooksLikeANormalStringDoYouWantToInputItAsAStr = 'This looks like a '
    +'normal string. Do you want to input it as a string ?';
  rsProtect = 'Protect';
  rsNoAccess = 'No Access';
  rsReadOnly = 'Read Only';
  rsReadWrite = 'Read/Write';
  rsWriteCopy = 'Write Copy';
  rsExecute = 'Execute';
  rsExecuteReadOnly = 'Execute/Read only';
  rsExecuteReadWrite = 'Execute/Read/Write';
  rsExecuteWriteCopy = 'Execute/Write Copy';
  rsGuarded = 'Guarded';
  rsNotCached = 'Not Cached';
  rsWriteCombine = 'Write Combine';
  rsBase = 'Base';
  rsSize = 'Size';
  rsPhysicalAddress = 'Physical Address';
  rsModule = 'Module';
  rsAddress = 'address';
  rsBytes = 'bytes';

function THexview.gethasSelection: boolean;
begin
  result:=fhasSelection or isEditing;
end;

function THexview.isLocked:boolean;
begin
  result:=fLockedTo<>nil;
end;

function THexview.isShowingDifference: boolean;
begin
  result:=fShowDiffHv<>nil;
end;

procedure THexView.Lock(hv: THexview);
begin
  unlock; //first unlock with other entries it might have
  fLockedTo:=hv;
  hv.fLockedTo:=self; //same unit class accessing private vars is allowed in pascal, quick whining

  fLockedToBaseAddress:=fAddress;
  hv.fLockedToBaseAddress:=hv.fAddress;
end;

procedure THexView.Unlock;
begin
  if fLockedTo<>nil then
    fLockedTo.fLockedTo:=nil;

  fLockedTo:=nil;
end;

procedure THexView.EndDifferenceView;
begin
  unlock;

  if fShowDiffhv<>nil then
  begin
    fShowDiffHv.fShowDiffHv:=nil;
    fShowDiffHv.update;

    fShowDiffHv:=nil;
  end;
  update;
end;

procedure THexView.ShowDifference(hv: THexview);
begin
  EndDifferenceView;

  if hv=self then raise exception.create(rsBigFuckingError);
  //set an addresslock between this and that hexview

  lock(hv);

  fShowDiffHv:=hv;
  hv.fShowDiffHv:=self;

  update;
  hv.update;
end;



procedure THexView.setBytesPerSeperator(b: integer);
begin
  if not (b in [2,4,8]) then
    raise exception.create(Format(rsInvalidBytesPerSeperatorValue, [inttostr(b)]
      ));

  fbytesPerSeperator:=b;
  update;
end;

procedure THexView.LockRowsize(size: integer=0);
begin
  if size=0 then
    flockedRowSize:=bytesPerLine
  else
    flockedRowSize:=size;
end;

procedure THexView.UnlockRowsize;
begin
  flockedRowSize:=0;
  hexviewResize(self);
  update;
end;

function THexView.CalculateGradientColor(Percentage: single; MaxColor, MinColor: TColor): TColor;
{
Calculates the color between MaxColor and MinColor
}
var newred, newgreen, newblue: dword;
c1,c2: TColor;
begin
  c2:=ColorToRGB(MaxColor);
  c1:=ColorToRGB(MinColor);

  newred:=trunc((graphics.Red(c1)*(1-(percentage/100))+graphics.Red(c2)*(percentage/100)));
  newgreen:=trunc((graphics.Green(c1)*(1-(percentage/100))+graphics.Green(c2)*(percentage/100)));
  newblue:=trunc((graphics.blue(c1)*(1-(percentage/100))+graphics.Blue(c2)*(percentage/100)));

  result:=RGBToColor(newred, newGreen, newBlue);
end;

procedure THexView.setCharEncoding(newce: TCharEncoding);
begin
  fCharEncoding:=newce;
  changelist.Clear;
  update;
end;

procedure THexView.setDisplayType(newdt: TDisplaytype);
begin
  fDisplayType:=newdt;

  if newdt=dtByteDec then
  begin
    byteSize:=offscreenbitmap.Canvas.TextWidth('XXX X'); //byte space and the character it represents
    byteSizeWithoutChar:=offscreenbitmap.Canvas.TextWidth('XXX ');
  end
  else
  begin
    byteSize:=offscreenbitmap.Canvas.TextWidth('XX X'); //byte space and the character it represents
    byteSizeWithoutChar:=offscreenbitmap.Canvas.TextWidth('XX ');
  end;


  if fDisplayType<>dtByte then
  begin
    isSelecting:=false;
    fhasSelection:=false;
    isEditing:=false;
  end;

  changelist.Clear;


  if fShowDiffHv<>nil then
  begin
    fShowDiffHv.fDisplayType:=newdt;
    if fDisplayType<>dtByte then
    begin
      fShowDiffHv.isSelecting:=false;
      fShowDiffHv.fhasSelection:=false;
      fShowDiffHv.isEditing:=false;
    end;

    fShowDiffHv.changelist.clear;
    fShowDiffHv.update;
  end;

  update;
  hexviewResize(self);
end;

procedure THexView.HandleEditKeyPress(wkey: TUTF8Char);
var
    b: byte;
    w: widestring;
    unreadable: boolean;
    bw: ptrUint;
    x: byte;

    s1,s2,s: string;

    vtype: TVariableType;
    hex: boolean;

    key: char;

begin
  if not isediting then exit;

  b:=getByte(selected,unreadable);
  if unreadable then
  begin
    if UseFileAsMemory then
    begin
      x:=0;
      writeprocessmemory(processhandle, pointer(selected),@x,1,bw);

      LoadMemoryRegion;

      b:=getByte(selected,unreadable);

      if unreadable then exit;
    end
    else
      exit; //unreadable
  end;

  key:=wkey[1];


  if (editingtype=hrByte) then
  begin
    if (not (key in ['a'..'f','A'..'F','0'..'9', #7, #8,'.',','])) then exit; //hex edit and not a hexadecimal value

    case fDisplayType of
      dtByte: s:=getByte(selected);
      dtByteDec: s:=getByteDec(selected, true);
      dtWord: s:=getWord(selected);
      dtWordDec: s:=getWordDec(selected, true);
      dtDword: s:=getDWord(selected);
      dtDwordDec: s:=getDWordDec(selected, true);
      dtQword: s:=getQWord(selected);
      dtQwordDec: s:=getQWordDec(selected, true);
      dtSingle: s:=getSingle(selected, true);
      dtDouble: s:=getDouble(selected, true);
    end;

    if (key in [#7, #8]) then
    begin
      if (fDisplayType in [dtByteDec, dtWordDec, dtDwordDec, dtQwordDec, dtSingle, dtDouble])  then
      begin
        if key=#7 then //delete
        begin
          if (length(s)>=editingcursorpos+1) and (s[editingcursorpos+1]=',') or (s[editingcursorpos+1]='.') then //delete from the entry after the decimal seperator
            s:=copy(s, 1, editingCursorPos+1)+copy(s, editingcursorpos+3, length(s))
          else
            s:=copy(s, 1, editingCursorPos)+copy(s, editingcursorpos+2, length(s)); //just delete

        end
        else //backspace
        begin
          if (length(s)>=editingcursorpos) and (s[editingcursorpos]=',') or (s[editingcursorpos]='.') then
            s:=copy(s, 1, editingCursorPos)+copy(s, editingcursorpos+2, length(s))
          else
            s:=copy(s, 1, editingCursorPos-1)+copy(s, editingcursorpos+1, length(s)); //just backspace
        end;
      end
      else
        exit; //the other types do not support these keys
    end
    else
    begin
      //replace the key with the provided one
      if (key in [',','.']) then
      begin
        if not (fDisplayType in [dtSingle, dtDouble]) then exit; //do , or . support for non float types
        s:=copy(s, 1, editingcursorpos);
      end
      else
      begin
        s1:=copy(s, 1, editingcursorpos);
        if (length(s)>=editingcursorpos+1) and (s[editingcursorpos+1]=',') or (s[editingcursorpos+1]='.') then //shift
          s2:=copy(s, editingcursorpos+1, length(s))
        else
          s2:=copy(s, editingcursorpos+2, length(s)); //replace
        s:=s1+ key+s2;
      end;

    end;

    hex:=true;

    case fDisplayType of
      dtByte: vtype:=vtByte;
      dtByteDec:
      begin
        vtype:=vtByte;
        hex:=false;
      end;

      dtWord: vtype:=vtWord;
      dtWordDec:
      begin
        vtype:=vtWord;
        hex:=false;
      end;

      dtDWord: vtype:=vtDword;
      dtDWordDec:
      begin
        vtype:=vtDword;
        hex:=false;
      end;

      dtQword: vtype:=vtQword;
      dtQWordDec:
      begin
        vtype:=vtQword;
        hex:=false;
      end;

      dtSingle:
      begin
        vtype:=vtSingle;
        hex:=false;
      end;

      dtDouble:
      begin
        vtype:=vtDouble;
        hex:=false;
      end;
    end;

    try
      ParseStringAndWriteToAddress(s, selected, vtype, hex);
      case key of
        #8: if editingCursorPos>0 then dec(editingCursorPos); //backspace
        #7: ; //do nothing with the cursor
        else
          inc(editingCursorPos);
      end;

    except
    end;

    if editingCursorPos>=length(s) then
    begin
      //at the end of the line
      if not (fDisplayType in [dtByteDec, dtWordDec, dtDwordDec, dtQwordDec, dtSingle, dtDouble]) then //if not a decimal type then go to the next address
      begin
        selected:=selected+DisplayTypeByteSize[fDisplayType];
        editingCursorPos:=0;
      end;
    end;
  end else
  begin
    if CharEncoding=ceAscii then
    begin
      WriteProcessMemory(processhandle, pointer(selected), @wkey[1],1, bw);
      inc(Selected);
    end
    else
    if CharEncoding=ceCodepage then
    begin
      s:=UTF8ToWinCP(wkey);

      WriteProcessMemory(processhandle, pointer(selected), @s[1],length(s), bw);
      inc(Selected);
    end
    else
    if CharEncoding=ceutf8 then
    begin
     // testcode: wkey:='한글';
      b:=Length(wkey);
      WriteProcessMemory(processhandle, pointer(selected), @wkey[1],b, bw);
      selected:=selected+b;
    end
    else
    if charencoding=ceUtf16 then
    begin
      w:=UTF8ToUTF16(wkey);

      WriteProcessMemory(processhandle, pointer(selected), @w[1], length(w)*2, bw);
      selected:=selected+2;
    end;
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
        address:=Address+afterOffset-column
      else
        address:=Address-beforeOffset-column;

      update;
    end;
  end;
end;

function THexView.getAddressFromCurrentMousePosition(var region: THexRegion): ptrUint;
var
  p: tpoint;
begin
  p:=screentoclient(mouse.cursorpos);

  result:=getAddressFromPosition(p.x,p.y,region);
end;

function THexView.getAddressFromPosition(x, y: integer; var region: THexRegion): ptrUint;
var row: integer;
    column: integer;
    byteclickpos: integer;
begin
  region:=hrInvalid;
  result:=0;

  //find what part is selected
  if y>2+textheight*2 then
  begin
    row:=(y-(2+textheight*2)) div (textheight+fspaceBetweenLines);

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

      result:=fAddress+bytesperline*row+column - (column mod DisplayTypeByteSize[fDisplayType]);
    end
    else
    if InRange(x,charstart,charstart+bytesperline*charsize) then
    begin
      //charclick
      column:=(x-charstart) div charsize;

      region:=hrChar;
      result:=fAddress+bytesperline*row+column;

      if CharEncoding=ceUtf16 then
        result:=result-(result mod 2);

    end;
  end; //else it's a headerclick

end;
  {
procedure THexView.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);

  HandleEditKeyPress(key);
end;  }

procedure THexView.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);

  HandleEditKeyPress(UTF8Key);
end;

procedure THexView.KeyDown(var Key: Word; Shift: TShiftState);
var b: byte;
x: ptrUint;

start, stop: ptruint;

gotoaddress: qword;
begin

  if (shift=[]) or (shift=[ssshift]) then
  begin
    case key of
      VK_DELETE:
      begin
        if isediting and (fDisplayType in [dtByteDec, dtWordDec, dtDwordDec, dtQwordDec, dtSingle, dtDouble]) then
          HandleEditKeyPress(chr(7)); //there's no delete char and I can't be assed to change the whole function to tak a virtual key

        key:=0;
        exit;
      end;

      VK_BACK:
      begin
        if isediting then
        begin
          if fDisplayType in [dtByteDec, dtWordDec, dtDwordDec, dtQwordDec, dtSingle, dtDouble] then
          begin
            //try to delete the selected character (note that single and double do not always co-operate)
            HandleEditKeyPress(chr(8));
            key:=0;
            exit;
          end;
        end
        else
        begin
          key:=0;
          back;
        end;
      end;

      VK_SPACE:
      begin
        key:=0;
        follow;
      end;

      VK_ESCAPE:
      begin
        isEditing:=false;
        update;
      end;

      vk_up:
      begin
        if isEditing then
        begin
          dec(selected,bytesPerLine);
          selected2:=selected+1;
        end
        else
          address:=address-bytesPerLine;


        update;
      end;

      vk_down:
      begin
        if isEditing then
        begin
          inc(selected,bytesPerLine);
          selected2:=selected+1;
        end
        else
          address:=address+bytesperline;

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
              selected:=selected-DisplayTypeByteSize[fDisplayType];
              case fDisplayType of
                dtByteDec: editingCursorPos:=length(getByteDec(selected));
                dtWordDec: editingCursorPos:=length(getWordDec(selected));
                dtDwordDec: editingCursorPos:=length(getDWordDec(selected));
                dtQwordDec: editingCursorPos:=length(getQWordDec(selected));
                dtSingle: editingCursorPos:=length(getSingle(selected));
                dtDouble: editingCursorPos:=length(getDouble(selected));
                else
                  editingCursorPos:=  DisplayTypeByteSize[fDisplayType]*2-1;
              end;


            end;
          end;

          selected2:=selected+1;
        end
        else
          address:=address-1;

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

            //get the length
            case fDisplayType of
              dtByteDec: x:=length(getByteDec(selected))+1;
              dtWordDec: x:=length(getWordDec(selected))+1;
              dtDwordDec: x:=length(getDWordDec(selected))+1; //+1 because we might allow backspace/adding
              dtQwordDec: x:=length(getQWordDec(selected))+1;
              dtSingle: x:=length(getSingle(selected))+1;
              dtDouble: x:=length(getDouble(selected))+1;
              else
                x:=2*DisplayTypeByteSize[fDisplayType]
            end;

            if editingCursorPos>=x then
            begin
              selected:=selected+DisplayTypeByteSize[fDisplayType];
              editingCursorPos:=0;
            end;
          end;
          selected2:=selected+1;
        end
        else
          address:=address+1;
        update;
      end;

      vk_prior:
      begin
        if isEditing then
          dec(selected,bytesPerLine*(totallines-1))
        else
          address:=address-bytesPerLine*(totallines-1);

        update;
      end;

      vk_next:
      begin
        if isEditing then
          inc(selected,bytesPerLine*(totallines-1))
        else
          address:=address+bytesPerLine*(totallines-1);

        update;
      end;

      VK_ADD,VK_SUBTRACT:
      begin
        if isEditing then
        begin
          b:=0;
          if (ReadProcessMemory(processhandle, pointer(selected),@b,1,x)) or (UseFileAsMemory)  then
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

  end;
 // else
  begin
    if (ssCtrl in shift) and (not (ssAlt in shift)) then
    begin
      case key of
        VK_1: DisplayType:=dtByte;
        VK_2: DisplayType:=dtByteDec;
        VK_3: DisplayType:=dtWord;
        VK_4: DisplayType:=dtWordDec;
        VK_5: DisplayType:=dtDword;
        VK_6: DisplayType:=dtDwordDec;
        VK_7: DisplayType:=dtQword;
        VK_8: DisplayType:=dtQwordDec;
        VK_9: DisplayType:=dtSingle;
        VK_0: DisplayType:=dtDouble;

        VK_RETURN:
        begin
          if useRelativeBase and (relativeBase=TopAddress) then
            useRelativeBase:=false
          else
          begin
            useRelativeBase:=true;
            relativeBase:=TopAddress;
          end;
          key:=0;

          update;
        end;
      end;
    end;

  end;
  RefocusIfNeeded;

  inherited KeyDown(key,shift);
end;


procedure THexView.AddSelectedAddressToCheatTable;
var Vartype: Tvariabletype;
begin
  if fhasSelection or isediting then //selected
  begin
    case fdisplaytype of
      dtByte, dtByteDec: VarType:=vtByte;
      dtWord, dtWordDec: Vartype:=vtWord;
      dtDword, dtDwordDec: Vartype:=vtDword;
      dtQword, dtQwordDec: vartype:=vtQword;
      dtSingle: vartype:=vtSingle;
      dtDouble: vartype:=vtDouble;
      else
        vartype:=vtDword;
    end;

    mainform.addresslist.addAddressManually(inttohex(selected,8), Vartype);
  end;

end;

function THexView.GetSelectionStart: ptruint;
begin
  result:=MinX(selected,selected2);
end;

function THexView.GetSelectionStop: ptruint;
begin
  result:=MaxX(selected,selected2);
end;

procedure THexView.GetSelectionRange(var start: ptruint; var stop: ptruint);
begin
  start:=GetSelectionStart;
  stop:=GetSelectionStop;
end;

procedure THexView.CopySelectionToClipboard;
var
  fromAddress, toAddress: ptrUint;
  s: string;
  b: Byte;
  unreadable: boolean;
  ss: TShiftState;

  bytes, chars: string;
  i: integer;
begin

  ss:=GetKeyShiftState;
  s:='';

  if isEditing or fhasSelection then
  begin
    GetSelectionRange(fromaddress, toAddress);

    if selectiontype=hrChar then
    begin
      while fromaddress<=toAddress do
      begin
        b:=getByte(fromAddress,unreadable);
        if (unreadable) or (b<32) then
        begin
          //invalid characters used, use hex instead
          selectiontype:=hrByte;
          CopySelectionToClipboard;
          exit;
        end
        else
          s:=s+chr(b);


        inc(fromAddress);
      end;

    end
    else
    begin

      toAddress:=toAddress+DisplayTypeByteSize[fDisplayType]-1;
      s:='';
      while fromaddress<=toAddress do
      begin
        case fDisplayType of
          dtByte: s:=s+getByte(fromaddress);
          dtByteDec: s:=s+getByteDec(fromaddress, true);
          dtWord: s:=s+getWord(fromaddress);
          dtWordDec: s:=s+getWordDec(fromaddress, true);
          dtDword: s:=s+getDWord(fromaddress);
          dtDwordDec: s:=s+getDWordDec(fromaddress, true);
          dtQword: s:=s+getQWord(fromaddress);
          dtQwordDec: s:=s+getQWordDec(fromaddress, true);
          dtSingle: s:=s+getSingle(fromaddress, true);
          dtDouble: s:=s+getDouble(fromaddress, true);
        end;

         //byte array
        //b:=getByte(fromAddress,unreadable);
        //if not unreadable then
        //begin
        //  s:=s+inttohex(b,2)+' ';
        //end
        //else
        //  s:=s+'?? ';

        inc(fromAddress, DisplayTypeByteSize[fDisplayType]);
        if fromaddress<=toAddress then
          s:=s+' ';
      end;

    end;

    Clipboard.AsText:=s;
  end;
end;

procedure THexView.PasteFromClipboard;
var s: string;
b: TBytes;
i: integer;

validbytes: integer;
fromAddress, toAddress: ptrUint;


begin
  if isEditing or fhasSelection then
  begin
    s:=clipboard.AsText;
    fromAddress:=MinX(selected,selected2);
    toAddress:=MaxX(selected,selected2);


    try
      ConvertStringToBytes(s,true, b);
      validbytes:=0;
      for i:=0 to length(b)-1 do
        if b[i]<>-1 then inc(validbytes);

      if validbytes>trunc(length(b) / 2) then
      begin
        //valid enough AOB string
        if selectionType=hrChar then
          if MessageDlg(rsThisLooksLikeAnArrayOfByteDoYouWantToInputItAsAHex,
            mtConfirmation, [mbyes, mbno], 0)=mryes then
            selectionType:=hrByte;
      end
      else
      begin
        //invalid AOB string
        if selectionType=hrByte then
          if MessageDlg(rsThisLooksLikeANormalStringDoYouWantToInputItAsAStr,
            mtConfirmation, [mbyes, mbno], 0)=mryes then
            selectiontype:=hrChar;

      end;
    except
      selectionType:=hrChar;
    end;




    if selectionType=hrChar then
    begin
      if (isEditing) or ((toAddress-FromAddress)>length(s)) then
        ToAddress:=FromAddress+length(s)-1;

      i:=1;
      while fromaddress<=ToAddress do
      begin
        setbyte(fromaddress,ord(s[i]));
        inc(i);
        inc(fromaddress);

      end;
    end
    else
    begin
      if (isEditing) or ((toAddress-FromAddress)>length(b)) then
        ToAddress:=FromAddress+length(b)-1;

      i:=0;
      while fromaddress<=ToAddress do
      begin
        if b[i]<>-1 then
          setbyte(fromaddress,b[i]);

        inc(i);
        inc(fromaddress);
      end;

    end;
    update;

  end;


end;

procedure THexView.ChangeSelected;
var unreadable: boolean;
begin
  if isEditing or fhasSelection then
  begin
    getByte(selected,unreadable);
    if unreadable then exit;

    with Tvaluechangeform.Create(application) do
    begin
      address:=selected;

      case fDisplayType of
        dtByte, dtByteDec: VarType:=vtByte;
        dtWord, dtWordDec: Vartype:=vtWord;
        dtDword, dtDwordDec: Vartype:=vtDword;
        dtQword, dtQwordDec: vartype:=vtQword;
        dtSingle: vartype:=vtSingle;
        dtDouble: vartype:=vtDouble;
      end;
      ShowModal;
    end;
    update;
  end;
end;

procedure THexView.mbCanvasDoubleClick(Sender: TObject);
var
  p: tpoint;
  allocrangestart: integer;
  allocrangestop: integer;

  baserangestart: integer;
  baserangestop: integer;
  mbi: TMEMORYBASICINFORMATION;
begin
  changeSelected;

  p:=mouse.CursorPos;
  p:=ScreenToClient(p);
  //doubleclick doesn't happen often, so can be slow
  if p.y<mbCanvas.Canvas.GetTextHeight(memoryInfo) then
  begin
    allocrangestart:=mbCanvas.Canvas.GetTextWidth(copy(memoryinfo,1,memoryInfo_allocationbasepos));
    allocrangestop:=mbCanvas.Canvas.GetTextWidth(copy(memoryinfo,1,memoryInfo_allocationbaseend));

    baserangestart:=mbCanvas.Canvas.GetTextWidth(copy(memoryinfo,1,memoryinfo_baseaddresspos));
    baserangestop:=mbCanvas.Canvas.GetTextWidth(copy(memoryinfo,1,memoryinfo_baseaddressend));

    Virtualqueryex(processhandle,pointer(fAddress),mbi,sizeof(mbi));

    if InRange(p.x,allocrangestart,allocrangestop) then
    begin
      history.Push(pointer(faddress));
      Address:=ptruint(mbi.AllocationBase);
      exit;
    end;

    if InRange(p.x,baserangestart,baserangestop) then
    begin
      history.Push(pointer(faddress));
      Address:=ptruint(mbi.BaseAddress);
      exit;
    end;

  end;

end;

procedure THexView.mbCanvasMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var hr: THexRegion;
a: ptrUint;
byteclickpos: integer;
begin


  if (button=mbleft) then
  begin
    if isSelecting then
    begin
      //let's emulate one more mosemove just to be sure
      mbCanvasMouseMove(sender, [ssLeft], x,y);

      //selected2 is now properly updated
      isSelecting:=false;

      a:=getAddressFromPosition(x,y,hr);
      if hr<>hrInvalid then
        selectionType:=hr;


      if selected=selected2 then
      begin
        if (a<>selected) or (hr=hrInvalid) then exit; //out of bounds exit

        isEditing:=true;
        fhasSelection:=false;

        {$ifdef EDITWHEREYOUCLICK}   //add this define if you wish to start the editor at the spot you click insetad of the start
        byteclickpos:=(x-bytestart)-(((x-bytestart) div (byteSizeWithoutChar*DisplayTypeByteSize[fDisplayType])) * bytesizeWithoutChar*DisplayTypeByteSize[fDisplayType]);
        editingCursorPos:=(byteclickpos div charsize);
        if editingCursorPos>DisplayTypeByteSize[fDisplayType]*2 then
          editingCursorPos:=DisplayTypeByteSize[fDisplayType]*2-1;
        {$endif}





        editingType:=hr;
        selectionType:=hr;
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
    begin
      selected2:=address;
      selectionType:=hr;
    end;

    update;
  end;
end;

procedure THexView.mbCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hr: THexRegion;
  oldselected: ptrUint;
  byteclickpos: integer;
  wasrightclick: boolean;
begin
  setfocus;

  if (button=mbRight) and (fhasSelection=false) then
  begin
    button:=mbLeft; //handle the rightclick as a selection if nothing is selected
    wasrightclick:=true;
  end else wasrightclick:=false;


  if (button=mbLeft) then
  begin
    fhasSelection:=false;
    oldselected:=selected;
    selected:=getAddressFromPosition(x,y,hr);
    selected2:=selected;

    if hr<>hrInvalid then
    begin

      fhasSelection:=true; //fDisplayType=dtByte;
      isSelecting:=true; //fDisplayType=dtByte; //only start selecting if the type is byte
      selectionType:=hr;

      if isEditing then
      begin
        if (oldselected<>selected) then
        begin
          isEditing:=false;
          editingCursorPos:=0;
        end
        else
        begin
          if hr=hrByte then
          begin
            //update the cursor position
            byteclickpos:=(x-bytestart)-(((x-bytestart) div (byteSizeWithoutChar*DisplayTypeByteSize[fDisplayType])) * bytesizeWithoutChar*DisplayTypeByteSize[fDisplayType]);
            editingCursorPos:=(byteclickpos div charsize);
            if editingCursorPos>DisplayTypeByteSize[fDisplayType]*2 then
              editingCursorPos:=DisplayTypeByteSize[fDisplayType]*2-1;

          end;
        end;
      end;


      if wasrightclick then  //imeadiatly follow by an emulated mouseup
        mbCanvasMouseUp(sender, button,shift,x,y);

    end else isEditing:=false;
  end;

  update;
end;

procedure THexView.lineUp(sender: tobject);
begin
  address:=address-bytesPerLine*floor(power(abs(verticalscrollbar.Position-50),1.01));
end;

procedure THexView.lineDown(sender: TObject);
begin
  address:=address+bytesPerLine*floor(power(abs(verticalscrollbar.Position-50),1.01));
end;



procedure THexview.updateScroller(speed: integer);
begin
  if (speed<>0) then
  begin
    if scrolltimer=nil then
      scrolltimer:=ttimer.create(self);

    //max speed is 50 (50 and -50)
    scrolltimer.Interval:=10+100-(abs(speed)*(100 div 50));

    if speed<0 then
      scrolltimer.OnTimer:=lineUp
    else
      scrolltimer.OnTimer:=lineDown;

    scrolltimer.enabled:=true;


  //showmessage(inttostr(speed))
  end
  else
  begin
    if scrolltimer<>nil then
      scrolltimer.enabled:=false;

  end;

end;

procedure THexView.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  delta: integer;
  shiftispressed: boolean;
begin

  shiftispressed:=GetBit(15, GetKeyState(VK_SHIFT))=1;

  SetFocus; //get the focus back

  case scrollcode of
    scLineUp:
    begin
      if shiftispressed then
        address:=address-1
      else
        address:=address-bytesPerLine;
    end;

    scLineDown:
    begin
      if shiftispressed then
        address:=address+1
      else
        address:=address+bytesPerLine;
    end;

    scPageDown: address:=address+bytesPerLine*(totallines-1);
    scPageUp:   address:=address-bytesPerLine*(totallines-1);
    sctrack:
    begin
      delta:=scrollpos-50;
      updatescroller(delta);
      exit;
//      address:=address+bytesPerLine*delta;
    end;

    scEndScroll:
    begin
      scrollpos:=50;
      updatescroller(0);
    end;
  end;




  update;
  scrollpos:=50;
end;

procedure THexView.MouseScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: integer;
  shiftispressed: boolean;
begin
  shiftispressed:=GetBit(15, GetKeyState(VK_SHIFT))=1;
  if shiftispressed then
  begin
    if WheelDelta>0 then
      address:=address-1
    else
      address:=address+1;

    handled:=true;
  end
  else
  begin
    if Focused then i:=2 else i:=4;
    if WheelDelta>0 then
      address:=address-(bytesPerLine*i)
    else
      address:=address+(bytesPerLine*i);
  end;

  update;
end;

procedure THexView.UpdateMemoryInfo;
var
  mbi: TMEMORYBASICINFORMATION;
  a: ptrUint;
  a64: qword;
  mi: TModuleInfo;
begin
  try
    zeromemory(@mbi,sizeof(mbi));
    Virtualqueryex(processhandle,pointer(fAddress),mbi,sizeof(mbi));
    memoryInfo:=rsProtect+':';

    if (mbi.Protect and PAGE_NOACCESS)>0 then memoryInfo:=memoryInfo+rsNoAccess+' ';
    if (mbi.Protect and PAGE_READONLY)>0 then memoryInfo:=memoryInfo+rsReadOnly+' ';
    if (mbi.Protect and PAGE_READWRITE)>0 then memoryInfo:=memoryInfo+rsReadWrite+' ';
    if (mbi.Protect and PAGE_WRITECOPY)>0 then memoryInfo:=memoryInfo+rsWriteCopy+' ';
    if (mbi.Protect and PAGE_EXECUTE)>0 then memoryInfo:=memoryInfo+rsExecute+' ';
    if (mbi.Protect and PAGE_EXECUTE_READ)>0 then memoryInfo:=memoryInfo+rsExecuteReadOnly+' ';
    if (mbi.Protect and PAGE_EXECUTE_READWRITE)>0 then memoryInfo:=memoryInfo+rsExecuteReadWrite+' ';
    if (mbi.Protect and PAGE_EXECUTE_WRITECOPY)>0 then memoryInfo:=memoryInfo+rsExecuteWriteCopy+' ';
    if (mbi.Protect and PAGE_GUARD)>0 then memoryInfo:=memoryInfo+rsGuarded+' ';
    if (mbi.Protect and PAGE_NOCACHE)>0 then memoryInfo:=memoryInfo+rsNotCached;
    if (mbi.Protect and PAGE_WRITECOMBINE)>0 then memoryInfo:=memoryInfo+rsWriteCombine;


    memoryInfo:=memoryInfo+' ';

    memoryInfo_allocationbasepos:=length(memoryinfo);
    memoryinfo:=memoryinfo+'AllocationBase='+IntToHex(ptrUint(mbi.AllocationBase), 8);
    memoryInfo_allocationbaseend:=length(memoryinfo);

    memoryinfo:=memoryinfo+' ';

    memoryinfo_baseaddresspos:=length(memoryinfo);
    memoryinfo:=memoryinfo+rsBase+'='+IntToHex(ptrUint(mbi.BaseAddress), 8);
    memoryinfo_baseaddressend:=length(memoryinfo);

    memoryinfo:=memoryinfo+' '+rsSize+'='+IntTohex(mbi.RegionSize, 1);

    if (formsettings<>nil) and assigned(GetPhysicalAddress) and formsettings.cbKernelOpenProcess.checked and GetPhysicalAddress(processhandle,pointer(fAddress),a64) then
      memoryInfo:=memoryInfo+' '+rsPhysicalAddress+'='+IntToHex(a64, 8);


    if symhandler.getmodulebyaddress(fAddress,mi) then
      memoryInfo:=memoryInfo+' '+rsModule+'='+mi.modulename;

  except
  end;
end;

procedure THexView.LoadMemoryRegion;
begin
  MemoryMap.clear; //erase the old data, memory access will fill it when needed
end;


function THexView.GetPageInfo(a: ptruint): PPageInfo;
var
    p: TPageInfo;
    x: ptrUint;
begin
  a:=a and (not $fff);
  if MemoryMapItterator.Locate(a) then
    result:=MemoryMapItterator.DataPtr
  else
  begin
    //get memory page info
    p.baseaddress:=a;
    p.readable:=readprocessmemory(processhandle, pointer(a), @p.data[0], 4096,x);
    if p.readable then
      p.inModule:=symhandler.inModule(a)
    else
      p.inModule:=false;

    memorymap.Add(a, p);
    MemoryMapItterator.Locate(a);
    result:=MemoryMapItterator.DataPtr;
  end;
end;

function THexView.inModule(a: ptrUint): boolean;
var
    pi: PPageInfo;
begin
  pi:=GetPageInfo(a);
  result:=pi.readable and pi.inModule;
end;

procedure THexView.setByte(a: ptrUint;value: byte);
var br: ptrUint;
begin
  WriteProcessMemory(processhandle, pointer(a),@value,1,br);
end;

function THexView.getByte(a: ptrUint; var unreadable: boolean): byte; overload;
var
  pi: PPageinfo;
  offset: word;
begin
  pi:=getPageInfo(a);

  offset:=a-pi.baseaddress;

  unreadable:=not pi.readable;
  if pi.readable then
    result:=pi.data[offset]
  else
    result:=0;
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

function THexView.getQWordValue(a: ptruint; out unreadable: boolean): qword;
var
  qw: qword;
  pqw: pbytearray;
  err,err2,err3,err4, err5, err6, err7, err8: boolean;
begin
  pqw:=@qw;
  pqw[0]:=getbyte(a,err);
  pqw[1]:=getbyte(a+1,err2);
  pqw[2]:=getbyte(a+2,err3);
  pqw[3]:=getbyte(a+3,err4);
  pqw[4]:=getbyte(a+4,err5);
  pqw[5]:=getbyte(a+5,err6);
  pqw[6]:=getbyte(a+6,err7);
  pqw[7]:=getbyte(a+7,err8);

  unreadable:=err or err2 or err3 or err4 or err5 or err6 or err7 or err8;
  result:=qw;
end;

function THexView.getQWord(a: ptrUint): string;
var
  qw: qword;
  err: boolean;
begin
  qw:=getqwordValue(a,err);
  if err then
    result:='????????????????'
  else
    result:=inttohex(qw,16);
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

function THexView.getByteDec(a: ptrUint; full: boolean=false): string;
var
  b: byte;
  pb: pbytearray;
  err: boolean;
begin
  b:=getbyte(a,err);

  if err then
    result:='???'
  else
    result:=inttostr(b);

  if (not full) and (length(result)>5) then
    result:=copy(result,1,2)+'...';
end;

function THexView.getWordDec(a: ptrUint; full: boolean=false): string;
var
  w: word;
  pw: pbytearray;
  err,err2: boolean;
begin
  pw:=@w;
  pw[0]:=getbyte(a,err);
  pw[1]:=getbyte(a+1,err2);

  if err or err2 then
    result:='???'
  else
    result:=inttostr(w);

  if (not full) and (length(result)>7) then
    result:=copy(result,1,4)+'...';
end;

function THexView.getDWordDec(a: ptrUint; full: boolean=false): string;
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

  if (not full) and (length(result)>11) then
    result:=copy(result,1,8)+'...';
end;

function THexView.getQWordDec(a: ptrUint; full: boolean=false): string;
var
  qw: qword;
  pqw: pbytearray;
  err,err2,err3,err4,err5,err6,err7,err8: boolean;
begin
  pqw:=@qw;
  pqw[0]:=getbyte(a,err);
  pqw[1]:=getbyte(a+1,err2);
  pqw[2]:=getbyte(a+2,err3);
  pqw[3]:=getbyte(a+3,err4);
  pqw[4]:=getbyte(a+4,err5);
  pqw[5]:=getbyte(a+5,err6);
  pqw[6]:=getbyte(a+6,err7);
  pqw[7]:=getbyte(a+7,err8);

  if err or err2 or err3 or err4 or err5 or err6 or err7 or err8 then
    result:='???'
  else
    result:=inttostr(qw);

  if (not full) and (length(result)>20) then
    result:=copy(result,1,18)+'...';
end;

function THexView.getSingle(a: ptrUint; full: boolean=false): string;
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


  if (not full) and (length(result)>11) then
    result:=copy(result,1,8)+'...';
end;

function THexView.getDouble(a: ptrUint; full: boolean=false): string;
var
  d: double;
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

  if (not full) and (length(result)>20) then
    result:=copy(result,1,18)+'...';
end;


function THexView.getChar(a: ptrUint; out charlength: integer): string;
var err: boolean;
    w: word;
    b,b2: byte;
    dw: dword;

    wc: widechar;

    ws: WideString;

    c: TUTF8Char;
    i,l: integer;

begin

  charlength:=1;
  b:=getbyte(a,err);
  if err then
  begin
    result:='?';
    exit;
  end;

  if fCharEncoding=ceAscii then
  begin
    if b in [0..31] then
      result:='.'
    else
      result:=chr(b);
  end
  else
  if fCharEncoding=ceCodepage then
  begin
    result:=chr(b);
  end
  else
  if fCharEncoding=ceutf8 then
  begin
    dw:=getDwordValue(a, err);

    if err then exit('?');

    l:=UTF8CharacterLength(pchar(@dw));
    if l=0 then l:=1;
    charlength:=l;

    setlength(result,l);
    CopyMemory(@result[1], @dw, l);

  end
  else
  if fCharEncoding=ceUtf16 then
  begin
    dw:=getDwordValue(a, err);

    if err then exit('?');

    l:=UTF16CharacterLength(pwidechar(@dw));
    if l=0 then l:=1;
    charlength:=l*sizeof(widechar);

    if l>1 then
    asm
      nop;
    end;
    setlength(ws, l);
    copymemory(@ws[1], @dw,l*sizeof(widechar));

    result:=ws;
    {
    b2:=getByte(a+1,err);
    if err then
      result:='?'
    else
    begin
      w:=(b2 shl 8)+b;
      wc:=widechar(w);

      result:=wc;
    end; }
  end;
end;

function THexView.getDwordValue(a: ptruint; out unreadable: boolean): dword;
var
  pdw: PByteArray;
begin
  pdw:=@result;
  pdw[0]:=getbyte(a,unreadable);
  if not unreadable then
    pdw[1]:=getbyte(a+1,unreadable);

  if not unreadable then
    pdw[2]:=getbyte(a+2,unreadable);

  if not unreadable then
    pdw[3]:=getbyte(a+3,unreadable);
end;

function THexView.getUTF8CharByteLength(a: ptruint): integer;
var
  dw: dword;

  err: boolean;
begin
  dw:=getDwordValue(a, err);
  if not err then
    result:=max(1,UTF8CharacterLength(pchar(@dw)))
  else
    result:=1;
end;

function THexView.getUTF16CharByteLength(a: ptruint): integer;
var
  dw: dword;

  err: boolean;
begin
  dw:=getDwordValue(a, err);
  if not err then
    result:=max(2,UTF16CharacterLength(pwidechar(@dw))*sizeof(widechar))
  else
    result:=2;
end;


procedure THexView.render;
var
  currentaddress: ptrUint;

  i,j: integer;
  x: word;
  cheader: string; //header for the char part
  bheader: string; //header for the byte part
  bytepos: integer;
  initialoffset: byte;
  seperators: array of integer;
  seperatorindex: integer;

  itemnr: integer;
  displaythis: boolean;

  seperatorshift: integer;
  seperatormask: integer;
  bps: integer;

  compareToAddress: ptruint;
  different: boolean;

  bp: PBreakpoint;


  char: string;
  nextCharAddress: ptruint;
  lastcharsize: integer;

  selectedcharsize: integer;

  unreadable: boolean;
  s: string;
  v_qword: int64;
  v_double: double absolute v_qword;
  v_byte: shortint absolute v_qword;
  v_word: smallint absolute v_qword;
  v_int: integer absolute v_qword;
  v_float: single absolute v_qword;

  displayOffset: ptruint;

begin
  displayOffset:=0;
  if bytesperline<=0 then exit;
  if Parent=nil then exit;

  if displayType=dtByte then
    bps:=fbytesPerSeperator
  else
    bps:=0;

  case bps of
    8: seperatorshift:=3;
    4: seperatorshift:=2;
    2: seperatorshift:=1;
    else seperatorshift:=0;
  end;
  seperatormask:=bps-1;


  setlength(seperators, bytesperline shr seperatorshift);
  seperatorindex:=0;

  currentaddress:=fAddress;
  nextCharAddress:=currentaddress;

  if not useRelativeBase then
    offscreenbitmap.Canvas.TextOut(0,0,memoryInfo)
  else
    offscreenbitmap.Canvas.TextOut(0,0,' '+inttohex(RelativeBase,8)+' : '+memoryInfo);

  offscreenbitmap.Canvas.TextOut(0, textheight, rsAddress);

  bheader:='';
  cheader:='';

  //(re)initialize the changelist (only has affect if the size is different)
  changelist.Initialize(currentAddress, totallines*bytesperline);
  different:=false;

  //create header
  initialoffset:=currentaddress and $f;


  if not UseRelativeBase then
  begin
    if usedRelativeBase then
    begin
      if fAddress<ptrUint($100000000) then
        addresswidth:=addresswidthdefault
      else
        addresswidth:=offscreenbitmap.Canvas.TextWidth(inttohex(fAddress,8));

      usedRelativeBase:=false; //only need to do this once (saves a small amount of cpu, actually neglible, but still...)
    end;
  end
  else
  begin
    if not usedRelativeBase then
    begin
      addresswidth:=offscreenbitmap.Canvas.TextWidth('+'+inttohex(fAddress,8));
      usedRelativeBase:=true;
    end;

    //displayOffset:=faddress-(fAddress+RelativeBase);
    displayOffset:=RelativeBase;
  end;

  bytestart:=addresswidth+8;

  charstart:=bytestart+bytesperline*byteSizeWithoutChar;


  for i:=0 to bytesperline-1 do
  begin
    case displayType of
      dtByte: bheader:=bHeader+inttohex(((currentaddress+i-displayOffset) and $ff),2)+' ';
      dtByteDec: bheader:=bHeader+inttohex(((currentaddress+i-displayOffset) and $ff),2)+'  ';
      dtWord, dtWordDec: if (i mod 2)=0 then bheader:=bHeader+inttohex(((currentaddress+i-displayOffset) and $ff),2)+' ' else bheader:=bHeader+'   ';
      dtDWord, dtDwordDec, dtSingle: if (i mod 4)=0 then bheader:=bHeader+inttohex(((currentaddress+i-displayOffset) and $ff),2)+' ' else bheader:=bHeader+'   ';
      dtQword, dtQwordDec, dtDouble: if (i mod 8)=0 then bheader:=bHeader+inttohex(((currentaddress+i-displayOffset) and $ff),2)+' ' else bheader:=bHeader+'   ';
    end;

    cheader:=cheader+inttohex((initialoffset+i) and $f,1);

    if (((initialoffset+i) and seperatormask)=seperatormask) and (i<>bytesperline-1) then
    begin
      seperators[seperatorindex]:=i;
      inc(seperatorindex);
    end;
  end;

//  bheader:='bpl='+inttostr(bytesperline)+' bytesize='+inttostr(bytesize)+' width='+inttostr(mbcanvas.width);
  offscreenbitmap.Canvas.TextOut(bytestart,textheight, bheader);
  offscreenbitmap.Canvas.TextOut(charstart,textheight, cheader);


  itemnr:=0;
  selectedcharsize:=1;

  if isEditing then
  begin

    case CharEncoding of
      ceAscii: selectedcharsize:=1;
      ceCodePage: selectedcharsize:=1;
      ceUtf8: selectedcharsize:=getUTF8CharByteLength(selected);
      ceUtf16: selectedcharsize:=getUTF16CharByteLength(selected);
    end;

  end;

  for i:=0 to totallines-1 do
  begin

    if UseRelativeBase then
    begin
      if currentaddress>=RelativeBase then
        offscreenbitmap.Canvas.TextOut(0, 2+2*textheight+(i*(textheight+fspaceBetweenLines)),'+'+inttohex(currentaddress-RelativeBase,8))
      else
        offscreenbitmap.Canvas.TextOut(0, 2+2*textheight+(i*(textheight+fspaceBetweenLines)),'-'+inttohex(RelativeBase-currentaddress,8));
    end
    else
      offscreenbitmap.Canvas.TextOut(0, 2+2*textheight+(i*(textheight+fspaceBetweenLines)),inttohex(currentaddress,8));

    bytepos:=0;
    for j:=0 to bytesperline-1 do
    begin
      if inModule(currentaddress) then
        offscreenbitmap.canvas.Font.Color:=clGreen
      else
        offscreenbitmap.canvas.Font.Color:=clWindowText;

      if fhasSelection and inrangex(currentaddress,minx(selected,selected2),maxx(selected,selected2)) then
        offscreenbitmap.canvas.Font.Color:=clRed;




      //if isEditing and ((currentAddress=selected) or ((editingtype=hrchar) and ((CharEncoding=ceUtf16) and (currentaddress=selected+1)))) then
      if isEditing and inrangex(currentAddress, selected, selected+selectedcharsize-1) then
      begin
        if (editingtype=hrByte) and (currentaddress=selected) then
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

      if debuggerthread<>nil then
      begin
        //check if the current address has a breakpoint
        bp:=debuggerthread.isBreakpoint(currentaddress);
        if bp<>nil then
        begin
          offscreenbitmap.canvas.Brush.Color:=clGreen;
          offscreenbitmap.canvas.Font.Color:=clBlack;
        end;
      end;



      //todo: refactor this

      displaythis:=false;
      case displayType of
        dtByte: begin changelist.values[itemnr]:=getByte(currentAddress); displaythis:=true; end;
        dtByteDec: begin changelist.values[itemnr]:=getByteDec(currentAddress); displaythis:=true; end;
        dtWord: if (j mod 2)=0 then begin changelist.values[itemnr]:=getWord(currentAddress); displaythis:=true; end;
        dtWordDec: if (j mod 2)=0 then begin changelist.values[itemnr]:=getWordDec(currentAddress); displaythis:=true; end;
        dtDWord: if (j mod 4)=0 then begin changelist.values[itemnr]:=getDWord(currentAddress); displaythis:=true; end;
        dtDWordDec: if (j mod 4)=0 then begin changelist.values[itemnr]:=getDWordDec(currentAddress); displaythis:=true; end;
        dtQWord: if (j mod 8)=0 then begin changelist.values[itemnr]:=getQWord(currentAddress); displaythis:=true; end;
        dtQWordDec: if (j mod 8)=0 then begin changelist.values[itemnr]:=getQWordDec(currentAddress); displaythis:=true; end;
        dtSingle: if (j mod 4)=0 then begin changelist.values[itemnr]:=getsingle(currentAddress); displaythis:=true; end;
        dtDouble: if (j mod 8)=0 then begin changelist.values[itemnr]:=getDouble(currentAddress); displaythis:=true; end;
      end;

      if fShowDiffHV<>nil then
      begin
        //slows it down a lot, especially fucks up the getByte function
        compareToAddress:=fShowDiffHv.fAddress+currentAddress-fAddress;

        case displayType of
          dtByte: different:=changelist.values[itemnr]<>fShowDiffHV.getByte(compareToAddress);
          dtByteDec: different:=changelist.values[itemnr]<>fShowDiffHV.getByteDec(compareToAddress);
          dtWord: if (j mod 2)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getWord(compareToAddress);
          dtWordDec: if (j mod 2)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getWordDec(compareToAddress);
          dtDWord: if (j mod 4)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getDWord(compareToAddress);
          dtDWordDec: if (j mod 4)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getDWordDec(compareToAddress);
          dtQWord: if (j mod 4)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getQWord(compareToAddress);
          dtQWordDec: if (j mod 4)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getQWordDec(compareToAddress);
          dtSingle: if (j mod 4)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getsingle(compareToAddress);
          dtDouble: if (j mod 8)=0 then different:=changelist.values[itemnr]<>fShowDiffHV.getDouble(compareToAddress);
        end;

        if different then
        begin
          offscreenbitmap.canvas.Font.Color:=clYellow;
          offscreenbitmap.canvas.Brush.Color:=clBlue;
        end;
      end;


      if gettickcount-changelist.LastChange[itemnr]<fadetimer then
      begin
//        offscreenbitmap.canvas.Brush.Color:=CalculateGradientColor((fadetimer-(gettickcount-changelist.LastChange[itemnr]))/10, clRed, offscreenbitmap.canvas.Brush.Color);
        offscreenbitmap.canvas.Brush.Color:=CalculateGradientColor((fadetimer-(gettickcount-changelist.LastChange[itemnr]))/(fadetimer div 100), clRed, offscreenbitmap.canvas.Brush.Color);
        if offscreenbitmap.canvas.Font.Color=clred then
          offscreenbitmap.canvas.Font.Color:=clBlue;
      end;

      if displaythis then
      begin
        s:=changelist.values[itemnr];
        if assigned(fOnValueRender) then
          fOnValueRender(self, currentaddress, s);

        offscreenbitmap.canvas.TextOut(bytestart+bytepos*charsize, 2+2*textheight+(i*(textheight+fspaceBetweenLines)) , s);
      end;


      //if isEditing and ((currentAddress=selected) or ((editingtype=hrByte) and ((CharEncoding=ceUtf16) and (currentaddress=selected+1)))) then
      if isEditing and inrangex(currentAddress, selected, selected+selectedcharsize-1) then
      begin
        if (editingtype=hrChar) and (currentaddress=selected) then
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

      if currentAddress=nextCharAddress then //(fCharEncoding in [ceAscii, ceUtf8]) or (j mod 2=0) then
      begin
        char:=getChar(currentAddress, lastcharsize);

        if assigned(fOnCharacterRender) then
          fOnCharacterRender(self, currentaddress, char);

        offscreenbitmap.canvas.TextOut(charstart+j*charsize, 2+2*textheight+(i*(textheight+fspaceBetweenLines)), char); //char

        inc(nextCharAddress, lastcharsize);
      end;


      offscreenbitmap.canvas.Font.Color:=clWindowText;
      offscreenbitmap.canvas.Brush.Color:=clBtnFace;

      if isEditing and (currentAddress=selected) then
      begin
        //render the carret
        offscreenbitmap.canvas.Pen.Width:=2;
        offscreenbitmap.canvas.Pen.Color:=clRed;
        if editingtype=hrByte then //draw the carret for the byte
          offscreenbitmap.Canvas.Line(1+bytestart+bytepos*charsize+editingCursorPos*charsize,2+2*textheight+(i*(textheight+fspaceBetweenLines))+1,1+bytestart+bytepos*charsize+editingCursorPos*charsize,2+2*textheight+(i*(textheight+fspaceBetweenLines))+textheight-2)
        else //draw the carret for the char
          offscreenbitmap.Canvas.Line(1+charstart+j*charsize,2+2*textheight+(i*(textheight+fspaceBetweenLines))+1,1+charstart+j*charsize,2+2*textheight+(i*(textheight+fspaceBetweenLines))+textheight-2);

        offscreenbitmap.canvas.Pen.Width:=1;

      end;

      bytepos:=bytepos+3;
      if DisplayType=dtByteDec then   //byte decimal is special as it has a big chance it's going to be bigegr than 99
        inc(bytepos);

      inc(currentaddress);
      inc(itemnr);
    end;
  end;



  for i:=0 to seperatorindex-1 do
  begin
    offscreenbitmap.Canvas.Pen.Color:=clYellow;
    offscreenbitmap.Canvas.PenPos:=point(bytestart+(seperators[i]+1)*byteSizeWithoutChar-(charsize shr 1),(textheight+fspaceBetweenLines));
    offscreenbitmap.Canvas.LineTo(bytestart+(seperators[i]+1)*byteSizeWithoutChar-(charsize shr 1),mbcanvas.height);

    offscreenbitmap.Canvas.PenPos:=point(charstart+(seperators[i]+1)*charsize,(textheight+fspaceBetweenLines));
    offscreenbitmap.Canvas.LineTo(charstart+(seperators[i]+1)*charsize,mbcanvas.height);
  end;

  offscreenbitmap.Canvas.Pen.Color:=clBlack;
  offscreenbitmap.Canvas.PenPos:=point(0,textheight*2);
  offscreenbitmap.Canvas.LineTo(charstart+bytesperline*charsize,textheight*2);


  v_qword:=int64(getQWordValue(SelectionStart, unreadable));
  if not unreadable then
    s:=format(': byte: %d word: %d integer: %d int64: %d float:%f double: %f',[integer(v_byte), integer(v_word), v_int, v_qword,v_float, v_double])
  else
    s:='';


  if selectionstart=0 then statusbar.SimpleText:='' else
  begin
    if selected<>selected2 then
      statusbar.SimpleText:=format('%.8x - %.8x (%d '+rsBytes+') %s',[SelectionStart, SelectionStop, SelectionStop-SelectionStart+1, s])
    else
      statusbar.SimpleText:=format('%.8x %s',[SelectionStart, s])
  end;

end;

procedure THexView.setAddress(a: ptrUint);
begin
  fAddress:=a;
  if changelist<>nil then
    changelist.Clear;

  if fShowDiffHv<>nil then
  begin
    LoadMemoryRegion;
    fShowDiffHv.LoadMemoryRegion;

    if fLockedTo=nil then
      fShowDiffHv.update;
  end;

  if fLockedTo<>nil then
  begin
    //first update the other
    fLockedTo.fAddress:=fLockedTo.fLockedToBaseAddress+(a-fLockedToBaseAddress);

    if fLockedTo.changelist<>nil then
      fLockedTo.changelist.clear;

    flockedTo.update;
    //and now for myself
  end;

  update;
end;

procedure THexView.hexviewResize(sender: TObject);
var oldsizex,oldsizey: integer;
    seperatorcount: integer;
begin
  if UseRelativeBase then
  begin
    addresswidth:=offscreenbitmap.Canvas.TextWidth('+'+inttohex(fAddress,8));
  end
  else
  begin
    {$ifdef cpu64}

    if fAddress<ptrUint($100000000) then
      addresswidth:=addresswidthdefault
    else
      addresswidth:=offscreenbitmap.Canvas.TextWidth(inttohex(fAddress,8));
    {$else}
    addresswidth:=addresswidthdefault;
    {$endif}
  end;

  oldsizex:=bytesperline;
  oldsizey:=totallines;

  bytestart:=addresswidth+8;

  usablewidth:=mbCanvas.ClientWidth-addresswidth-8;

  if flockedRowSize>0 then
    bytesPerLine:=flockedRowSize
  else
    bytesPerLine:=(usablewidth div bytesize) and $fffffff8;

  if bytesperline<=0 then
    bytesperline:=8;

  charstart:=bytestart+bytesperline*byteSizeWithoutChar;


  totallines:=1+(mbCanvas.clientHeight-(2+textheight*2)) div (textheight+fspaceBetweenLines);  //-(textheight*2) for the header
  if totallines<=0 then
    totallines:=1;


  if (oldsizex<>bytesperline) or (oldsizey<>totallines) then
    update;
end;

procedure THexView.update;
var oldAddressWidth: integer;
{$ifdef cpu64}
defaultrange: ptrUint;
{$endif}
begin
  //inherited update;

  if offscreenbitmap<>nil then
  begin
    if offscreenbitmap.Width<mbcanvas.width then
      offscreenbitmap.Width:=mbcanvas.width;

    if offscreenbitmap.Height<mbCanvas.width then
      offscreenbitmap.Height:=mbcanvas.Height;

    offscreenbitmap.Canvas.Brush.Color:=clBtnFace;
    offscreenbitmap.Canvas.FillRect(mbcanvas.ClientRect);

    oldAddressWidth:=addresswidth;
    {$ifdef cpu64}
    defaultrange:=UINT_PTR($100000000);
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

  if (lastaddress<>faddress) and assigned(fonAddressChange) then
    fonAddressChange(self, faddress);

  lastaddress:=fAddress;

  if ((lastselection1<>selected) or (lastselection2<>selected2)) and assigned(fOnByteSelect) then
    fOnByteSelect(self, selected, selected2);

  lastselection1:=selected;
  lastselection2:=selected2;
end;

procedure THexView.OnLostFocus(sender: TObject);
begin
  self.SetFocus;
end;

procedure THexView.AddToBackList(address: pointer);
begin
  backlist.Push(address);
end;

function THexView.hasBackList: boolean;
begin
  result:=backlist.Count>0;
end;

procedure THexView.Back;
begin
  if (backlist.Count>0) then //not editing and something in the backlist
    address:=qword(backlist.Pop);
end;

function THexView.CanFollow: boolean;
var start,stop: ptruint;
begin
  result:=false;
  if hasSelection then
  begin
    GetSelectionRange(start, stop);
    result:=(stop-start)+DisplayTypeByteSize[fdisplaytype]=processhandler.pointersize;
  end;
end;

procedure THexView.Follow;
var
  gotoaddress: ptruint;
  x: ptruint;
  mb: TMemoryBrowser;
begin
  if canfollow then
  begin
    //go to this selected address
    gotoaddress:=0;

    if ReadProcessMemory(processhandle, pointer(getSelectionStart), @gotoaddress, processhandler.pointersize,x) then
    begin
      //save the current address in the history
      if ssshift in GetKeyShiftState then
      begin
        //spawn a new memoryview window and set the address to there
        memorybrowser.Newwindow1.Click;
        mb:=TMemoryBrowser(MemoryBrowsers[memorybrowsers.count-1]);
        mb.hexview.Address:=gotoaddress;
        mb.show;
      end
      else
      begin
        backlist.push(pointer(address));

        //and go to this new address
        address:=gotoaddress;
        fhasSelection:=false;
        isEditing:=false;
      end;
    end;
  end;

end;

procedure THexView.mbPaint(sender: TObject);
var cr: Trect;
begin
  cr:=mbcanvas.Canvas.ClipRect;
  mbcanvas.Canvas.CopyRect(cr,offscreenbitmap.Canvas,cr);
end;


procedure THexview.setHexFont(f: TFont);
begin
  fHexFont.Assign(f);

  offscreenBitmap.Canvas.Font.Assign(fHexFont);
  mbCanvas.Font.Assign(fHexFont);

  textheight:=offscreenbitmap.Canvas.TextHeight('X?');
  addresswidthdefault:=offscreenbitmap.Canvas.TextWidth('XXXXXXXX');

  charsize:=offscreenbitmap.Canvas.TextWidth('X');
  byteSize:=offscreenbitmap.Canvas.TextWidth('XX X'); //byte space and the character it represents
  byteSizeWithoutChar:=offscreenbitmap.Canvas.TextWidth('XX ');

  hexviewResize(self);
  update;
end;

destructor THexview.destroy;
begin
  unlock; //always destroy links
  EndDifferenceView;

  if changelist<>nil then
    freeandnil(changelist);

  if verticalscrollbar<>nil then
    freeandnil(verticalscrollbar);

  if mbCanvas<>nil then
    freeandnil(mbCanvas);

  if offscreenbitmap<>nil then
    freeandnil(offscreenbitmap);

  if MemoryMapItterator<>nil then
    freeandnil(memorymapitterator);

  if MemoryMap<>nil then
    freeandnil(memorymap);


  inherited destroy;
end;



constructor THexView.create(AOwner: TComponent);
var sp: TStatusPanel;
begin
  inherited create(AOwner);

  fadetimer:=1000;
  backlist:=TStack.create;

  DoubleBuffered:=true; // http://cheatengine.org/mantis/view.php?id=280 , no effect for me, but should help those with no theme

  MemoryMap:=TMap.create(ituPtrSize, sizeof(TPageinfo));
  MemoryMapItterator:=TMapIterator.create(MemoryMap);

  changelist:=TChangelist.create;

  bytesPerSeperator:=8;

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

  fHexFont:=tfont.create;
  fHexFont.Charset:=DEFAULT_CHARSET;
  fHexFont.Color:=clwindowText;
  fHexFont.Height:=GetFontData(MainForm.font.handle).Height;
  if fHexFont.Height>-13 then
    fHexFont.Height:=-13;

  fHexFont.Name:='Courier New';
  fHexFont.Style:=[];

  statusbar:=TStatusBar.Create(self);
  statusbar.ParentFont:=true;
  statusbar.AutoSize:=false;
  statusbar.Name:='statusbar';
  statusbar.SimplePanel:=true;
  statusbar.align:=alBottom;
  statusbar.parent:=self;

  statusbar.simpletext:='Selection: <none>';





  mbCanvas:=TPaintbox.Create(self);
  with mbCanvas do
  begin
    align:=alClient;
    ParentFont:=False;
    Font.Assign(fHexFont);
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
  offscreenbitmap.canvas.font.Assign(fHexFont);

  textheight:=offscreenbitmap.Canvas.TextHeight('X?');
  addresswidthdefault:=offscreenbitmap.Canvas.TextWidth('XXXXXXXX');

  charsize:=offscreenbitmap.Canvas.TextWidth('X');
  byteSize:=offscreenbitmap.Canvas.TextWidth('XX X'); //byte space and the character it represents
  byteSizeWithoutChar:=offscreenbitmap.Canvas.TextWidth('XX ');

  update;
end;

end.

