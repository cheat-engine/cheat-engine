unit frmMemoryViewExUnit;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport, LCLType, LCLIntf,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, memdisplay, newkernelhandler, cefuncproc,
  syncobjs, math, savedscanhandler, foundlisthelper, CustomTypeHandler,
  symbolhandler, inputboxtopunit, commonTypeDefs, GL, GLext, Types, DPIHelper;


type TMVCompareMethod=(cmOr, cmXor, cmAnd);

type
  TMemoryDataSource=class(TThread)
  private
    cs: TCriticalSection;
    address: ptruint;
    buf: pbytearray;
    bufsize: integer;
    faddresslistonly: boolean;
    fcompareagainstsavedscan: boolean;
    fvartype: TVariableType;
    fvarsize: integer;

    comparemethod: TMVCompareMethod;

    temppagebuf: pbytearray;

    addresslist: TFoundList;
    previousvaluelist: TSavedScanHandler;
    ct: TCustomtype;



  public
    procedure lock;
    procedure unlock;
    procedure setRegion(address: ptruint; buf: pointer; size: integer);
    procedure execute; override;
    procedure fetchmem;
    procedure setaddresslist(state: boolean; listname: string);
    procedure setcompare(state: boolean; compareMethod: TMVCompareMethod; listname: string);
    constructor create(suspended: boolean);
  end;

  TMemEntry = record
    address:PtrUInt;
    size:integer;
  end;

  { TfrmMemoryViewEx }

  TfrmMemoryViewEx = class(TForm)
    btnMEMMAP: TButton;
    cbAddresslist: TComboBox;
    cbAddresslistOnly: TCheckBox;
    cbColor: TComboBox;
    cbCompare: TCheckBox;
    cbSavedList: TComboBox;
    cbType: TComboBox;
    edtAddress: TEdit;
    edtPitch: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblZOOM: TLabel;
    lblAddress: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pbMEM: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rbAnd: TRadioButton;
    rbOr: TRadioButton;
    rbXor: TRadioButton;
    sbVERT: TScrollBar;
    tbPitch: TTrackBar;
    tbZoom: TTrackBar;
    procedure btnMEMMAPClick(Sender: TObject);
    procedure cbAddresslistChange(Sender: TObject);
    procedure cbAddresslistOnlyChange(Sender: TObject);
    procedure cbAddresslistDropDown(Sender: TObject);
    procedure cbCompareChange(Sender: TObject);
    procedure cbSavedListChange(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure edtAddressKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure edtPitchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure pbMEMPaint(Sender: TObject);
    procedure sbVERTChange(Sender: TObject);
    procedure sbVERTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbPitchChange(Sender: TObject);
    procedure tbZoomChange(Sender: TObject);

    Procedure mdMouseDown(Sender:Tobject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mdMouseMove(Sender:Tobject;Shift: TShiftState; X, Y: Integer);
    procedure mdMouseUp(Sender:Tobject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mdMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure mdRecenterDrag;
  private
    buf: pbytearray;
    bufsize: integer;
    datasource: TMemoryDataSource;
    showing_help:boolean;
    mem_map:TList;
    procedure Panel1DblClick(Sender: TObject);
    function getCompareMethod: TMVCompareMethod;
    function ondata(newAddress: ptruint; PreferedMinimumSize: integer; var newbase: pointer; var newsize: integer): boolean;
    procedure UpdateZoomText(val:single);
    procedure UpdateAddress(val:PtrUInt);
    procedure UpdateZoomBar(val:single);
    procedure UpdateScrollbar(val:PtrUInt);
    procedure ClearMemMap;
    procedure MDResize;
  public
    md: TMemDisplay;
    procedure DisplayHelp;
  end;


implementation

uses MemoryBrowserFormUnit, MainUnit, ProcessHandlerUnit;

{$R *.lfm}

{ TMemoryDataSource }

constructor TMemoryDataSource.create(suspended: boolean);
begin
  cs:=tcriticalsection.create;

  getmem(temppagebuf, 4096);  //so it doesn't need to be allocated/freed each fetchmem call

  inherited create(suspended);
end;

procedure TMemoryDataSource.setcompare(state: boolean; compareMethod: TMVCompareMethod; listname: string);
begin
  if state then
  begin

    cs.Enter;
    try
      if previousvaluelist<>nil then
        freeandnil(previousvaluelist);

      previousvaluelist:=TSavedScanHandler.create(mainform.memscan.GetScanFolder, listname);
      previousvaluelist.AllowNotFound:=true;
      previousvaluelist.AllowRandomAccess:=true;

      self.compareMethod:=comparemethod;

    finally
      cs.Leave;
    end;

  end;

  fcompareagainstsavedscan:=state;
end;

procedure TMemoryDataSource.setaddresslist(state: boolean; listname: string);
begin
  if state then
  begin
    //Open a "PreviousValue object for the current memscan results.

    cs.Enter;

    try
      if addresslist<>nil then
        freeandnil(addresslist);

      addresslist:=TFoundList.create(nil, mainform.memscan, listname);
      addresslist.Initialize;
      fvartype:=mainform.memscan.VarType;
      ct:=mainform.memscan.CustomType;
      fvarsize:=mainform.memscan.Getbinarysize div 8;

    finally
      cs.leave;
    end;
  end;


  faddresslistonly:=state;

  fetchmem; //update now
end;

procedure TMemoryDataSource.fetchmem;
var x: ptrUint;
  a,a2: ptruint;
  s: integer;

  s2: integer;

  p: PByteArray;
  i: qword;
  j: integer;


  toread: integer;
begin
  lock;

  if buf<>nil then  //not yet initialized
  begin

    a:=address;
    i:=qword(-1);
    if faddresslistonly then
      i:=addresslist.FindClosestAddress(address-fvarsize+1); //all following accesses will be sequential


    toread:=bufsize;


    //while a<address+bufsize do

    while toread>0 do
    begin
      s:=minX((address+bufsize)-a, 4096-(a mod 4096)); //the number of bytes left in this page or for this buffer

      x:=0;
      if faddresslistonly then
      begin
        //check if this page has any addresses.
        zeromemory(@buf[a-address], s);

        if int64(i)<>-1 then
        begin
          a2:=addresslist.GetAddress(i);
          //get the first addresses that belong to this page (or has bytes in it)
          while (i<addresslist.count-1) and (a2<a-fvarsize+1) do
          begin
            inc(i);
            a2:=addresslist.GetAddress(i)
          end;

          while (i<addresslist.count-1) and (a2<a+s) do
          begin
            //render to the buffer
            s2:=fvarsize;
            if integer(a2-a)<0 then //cut off if it falls before the region
            begin
              dec(s2, integer(a2-a));
              inc(a2, integer(a2-a));
            end;

            if (a2-a+s2)>s then //cut off if it falls after the region
            begin
              s2:=s-(a2-a);
            end;

            if s2>0 then
            begin
              x:=0;
              ReadProcessMemory(processhandle, pointer(a2), @buf[a2-address], s2, x);

              if fcompareagainstsavedscan and (previousvaluelist<>nil) then
              begin
                //get the saved scan
                p:=previousvaluelist.getpointertoaddress(a2, fvartype, ct);
                if p<>nil then
                begin
                  case comparemethod of
                    cmor: for j:=0 to x-1 do buf[a2-address+j]:=buf[a2-address+j] or p[j];
                    cmxor: for j:=0 to x-1 do buf[a2-address+j]:=buf[a2-address+j] xor p[j];
                    cmand: for j:=0 to x-1 do buf[a2-address+j]:=buf[a2-address+j] and p[j];

                  end;


                end;

              end;

              inc(i);
              a2:=addresslist.GetAddress(i);
            end;
          end;
        end;

      end
      else
      begin
        ReadProcessMemory(processhandle, pointer(a), @buf[a-address], s, x);
        if x<s then //zero the unread bytes
          zeromemory(@buf[a-address], s-x);
      end;

      dec(toread,s);

      a:=a+s; //next page
    end;

  end;
  unlock;
end;

procedure TMemoryDataSource.execute;
begin
  while not terminated do
  begin
    sleep(100);

    fetchmem;
  end;
end;

procedure TMemoryDataSource.lock;
begin
  cs.enter
end;

procedure TMemoryDataSource.unlock;
begin
  cs.leave;
end;

procedure TMemoryDataSource.setRegion(address: ptruint; buf: pointer; size: integer);
begin
  lock;
  self.address:=address;
  self.buf:=buf;
  bufsize:=size;

  fetchmem;
  unlock;


end;

{ TfrmMemoryViewEx }
function TfrmMemoryViewEx.ondata(newAddress: ptruint; PreferedMinimumSize: integer; var newbase: pointer; var newsize: integer): boolean;
begin
  datasource.lock;
  if bufsize<PreferedMinimumSize then
  begin
    try
      ReAllocMem(buf, PreferedMinimumSize+4096);
    except
      beep;
    end;

    if buf=nil then
      bufsize:=0
    else
      bufsize:=PreferedMinimumSize+4096;
  end;

  datasource.setRegion(newaddress, buf, bufsize);
  datasource.unlock;

  newbase:=buf;
  newsize:=bufsize;
  result:=newsize>=PreferedMinimumSize; //allow the move if allocated enough memory
end;

procedure TfrmMemoryViewEx.FormCreate(Sender: TObject);
begin
  //create a datasource thread
  mem_map:=Tlist.Create;

  datasource:=TMemoryDataSource.create(true); //possible to add multiple readers in the future

  md:=TMemDisplay.Create(self);
  md.onData:=ondata;
  md.OnMouseDown:=mdMouseDown;
  md.OnMouseUp:=mdMouseUp;
  md.OnMouseMove:=mdMouseMove;
  md.OnMouseWheel:=mdMouseWheel;
  md.OnDblClick:=Panel1DblClick;
  md.OnClick:=Panel1Click;
  md.setPitch(1024);
  md.zoom:=1;
  md.Align:=alClient;
  md.parent:=panel1;

  bufsize:=panel1.Height*md.pitch;
  getmem(buf,bufsize);

  datasource.setRegion(MemoryBrowser.hexview.Address and ptruint(not $FFF), buf, bufsize);
  md.setPointer(MemoryBrowser.hexview.Address and ptruint(not $FFF), buf, bufsize);

  UpdateZoomBar(md.zoom);
  UpdateZoomText(md.zoom);
  UpdateAddress(md.address);
  edtPitch.Text:=IntTostr(md.pitch);
  tbPitch.Position:=md.pitch;

  datasource.Start;
end;

procedure TfrmMemoryViewEx.MDResize;
var psize,newsize: integer;
  newp:Pointer;
  a:PtrUInt;
begin
  if(not Assigned(md))then
    exit;
  if(not Assigned(datasource))then
    exit;

  psize:=(trunc(md.height / md.zoom)+16)*md.pitch;
  a:=md.address;

  if(assigned(md.fOnData) and md.fOnData(a,psize,newp,newsize))then
  begin
    md.p:=newp;
    md.size:=newsize;
  end;
end;

procedure TfrmMemoryViewEx.FormShow(Sender: TObject);
begin
    MDResize;

    AdjustComboboxSize(cbAddresslist, canvas);
    AdjustComboboxSize(cbSavedList, canvas);

end;

procedure TfrmMemoryViewEx.FormResize(Sender: TObject);
begin
    MDResize;
end;

procedure TfrmMemoryViewEx.ClearMemMap;
var i:integer;
  entry:^TMemEntry;
begin
  if(not Assigned(mem_map))then
    exit;
  for i:=0 to mem_map.Count-1 do
  begin
    entry:=mem_map[i];
    Dispose(entry);
  end;
  mem_map.Clear;
end;

procedure TfrmMemoryViewEx.FormDestroy(Sender: TObject);
begin
  if datasource<>nil then
  begin
    datasource.Terminate;
    datasource.WaitFor;
    freeandnil(datasource);
  end;
  ClearMemMap;
end;

procedure TfrmMemoryViewEx.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key=VK_F1)then
  begin
    if(not showing_help)then
    begin
      showing_help:=true;
      DisplayHelp;
      showing_help:=false;
    end;
  end
  else if(key=VK_ESCAPE)then
    self.close
  else if(key=VK_F5)then
    edtAddress.SetFocus
  else if(key=Ord('g'))then
  begin
    if(ssCtrl in Shift)then
        edtAddress.SetFocus;
  end;
end;

procedure TfrmMemoryViewEx.Panel1Click(Sender: TObject);
begin
  sbVERT.SetFocus;
end;

procedure TfrmMemoryViewEx.pbMEMPaint(Sender: TObject);
var i:integer;
  entry:^TMemEntry;
  max,a,b:Double;
  y,y2,tmp,w:integer;
  lasty,lasty2:integer;
begin
    if(not Assigned(mem_map))then
        exit;
    max:=High(UINT);
    if(processhandler.is64Bit)then
        max:=High(PtrUint);
    max:=max / pbMEM.Height;
    if(max=0)then
        exit;
    lasty:=-1;
    lasty2:=-1;
    pbMEM.Canvas.Brush.Color:=clDefault;
    pbMEM.Canvas.Clear;
    pbMEM.Canvas.Brush.Color:=clRed;
    w:=pbMEM.Width;
    for i:=0 to mem_map.Count-1 do
    begin
        entry:=mem_map[i];
        a:=entry.address;
        b:=a / max;
        y:=Trunc(b);
        b:=(a+entry.size)/max;
        y2:=trunc(b);
        if(y=lasty)then
        begin
          if(y2=lasty2)then
            continue;
        end;
        tmp:=y2;
        if(y2=y)then
            tmp:=y+1;
        pbMEM.Canvas.FillRect(0,y,w,tmp);
        lasty:=y;
        lasty2:=y2;
    end;
end;

procedure TfrmMemoryViewEx.sbVERTChange(Sender: TObject);
var i:integer;
  x,y:PtrUInt;
begin
  if(not Assigned(processhandler))then
    exit;
  y:=sbVERT.Max;
  if(y=0)then
    exit;
  x:=$FFFFFFFF;
  if(processhandler.is64Bit)then
    x:=PtrUInt(-1);
  x:=x div y;
  i:=sbVERT.Position;
  x:=x*i;
  md.MoveTo(0,0);
  md.setPointer(x);
  UpdateAddress(x);
end;

procedure TfrmMemoryViewEx.sbVERTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i:integer;
  start_address,end_address,next_address:PtrUInt;
  entry:^TMemEntry;
  found:boolean;
  next:boolean;
  function is_range_valid(a,b:PtrUInt):boolean;
  var i:integer;
  begin
    result:=false;
    for i:=0 to mem_map.Count-1 do
    begin
        entry:=mem_map[i];
        if(entry.address >= a)then
        begin
            if(entry.address <= b)then
            begin
              result:=true;
              break;
            end;
        end;
        if((entry.address+entry.size) >= a)then
        begin
            if((entry.address+entry.size) <= b)then
            begin
              result:=true;
              break;
            end;
        end;
        if(a >= entry.address)then
        begin
          if(b <= (entry.address+entry.size))then
          begin
            result:=true;
            break;
          end;
        end;
    end;
  end;
  procedure do_normal_scroll;
  var delta:PtrUInt;
    a,b,c:LongWord;
  begin
    if(processhandler.is64Bit)then
    begin
        delta:=end_address-start_address;
    end
    else
    begin
      a:=start_address;
      b:=end_address;
      c:=b-a;
      delta:=c;
    end;
    case key of
        VK_RIGHT,VK_DOWN: delta:=delta div 4;
        VK_LEFT,VK_UP: delta:=-delta div 4;
        VK_NEXT: delta:=(delta*15) div 16;
        VK_PRIOR: delta:=-(delta*15) div 16;
    end;
    next_address:=start_address+delta;
  end;

begin
  if(not(key in [VK_NEXT,VK_PRIOR,VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]))then
    exit;
  if(not assigned(mem_map))then
    exit;


  found:=false;

  start_address:=md.getAddressFromScreenPosition(0,0);
  end_address:=md.getAddressFromScreenPosition(0,md.Height-1);

  next_address:=start_address;


  if(is_range_valid(start_address,end_address) or (mem_map.Count=0))then
  begin
      do_normal_scroll;
      found:=true;
  end
  else
  begin
      next_address:=start_address;
      next:=false;
      if(key in [VK_NEXT,VK_RIGHT,VK_DOWN])then
        next:=true;

      if(next)then
      begin
          for i:=0 to mem_map.Count-1 do
          begin
            entry:=mem_map[i];
            if(entry.address>end_address)then
            begin
                next_address:=entry.address;
                found:=true;
                break;
            end;
          end;
          if(not found)then
          begin
            if(mem_map.Count>0)then
            begin
                entry:=mem_map[0];
                next_address:=entry.address;
            end
            else
                do_normal_scroll;
            found:=true;
          end;
      end
      else
      begin
          for i:=mem_map.Count-1 downto 0 do
          begin
            entry:=mem_map[i];
            if((entry.address+entry.size)<start_address)then
            begin
                next_address:=entry.address+entry.size;
                found:=true;
                break;
            end;
          end;
          if(not found)then
          begin
            if(mem_map.Count>0)then
            begin
              entry:=mem_map[mem_map.Count-1];
              next_address:=entry.address;
            end
            else
                next_address:=0;
            found:=true;
          end;
      end;
  end;

  if(found)then
  begin
      md.MoveTo(0,0);
      md.setPointer(next_address);
      UpdateAddress(next_address);
      UpdateScrollbar(next_address);
      key:=0;
  end;
end;

procedure TfrmMemoryViewEx.cbAddresslistOnlyChange(Sender: TObject);
begin
  cbAddresslist.enabled:=cbAddresslistOnly.checked;
  cbCompare.Enabled:=cbAddresslistOnly.checked;
  cbSavedList.enabled:=cbAddresslistOnly.checked;

  if datasource<>nil then
    datasource.setaddresslist(cbAddresslistOnly.checked, 'TMP');
end;

procedure TfrmMemoryViewEx.cbAddresslistChange(Sender: TObject);
begin
  if cbAddresslist.ItemIndex=0 then
    datasource.setaddresslist(cbAddresslistOnly.checked, 'TMP')
  else
    datasource.setaddresslist(cbAddresslistOnly.checked, cbAddresslist.text);
end;

procedure TfrmMemoryViewEx.btnMEMMAPClick(Sender: TObject);
var
  mbi : _MEMORY_BASIC_INFORMATION;
  address: PtrUInt;
  mappedfilename: string;
  kernelmode: boolean=false;
  entry:^TMemEntry;
begin
  if(not Assigned(md))then
    exit;
  if(not Assigned(mem_map))then
    exit;

  if DBKLoaded then
    kernelmode:=ssCtrl in GetKeyShiftState;

  ClearMemMap;

  address:=0;
  mbi.RegionSize:=$1000;
  while (GetRegionInfo(processhandle,pointer(address),mbi,sizeof(mbi), mappedfilename)<>0) and ((address+mbi.RegionSize)>address) do
  begin
    if(mbi.State=MEM_COMMIT)then
    begin
      New(entry);
      entry.address:=address;
      entry.size:=mbi.RegionSize;
      mem_map.Add(Pointer(entry));
    end;

    inc(address,mbi.RegionSize);
    if not kernelmode then
    begin
      if processhandler.is64Bit then
      begin
        {$IFDEF CPU64}
        if (address>=QWORD($8000000000000000)) then exit;
        {$ENDIF}
      end
      else
        if (address>=$80000000) then exit;
    end;
  end;
  pbMEM.Repaint;
end;

procedure TfrmMemoryViewEx.cbAddresslistDropDown(Sender: TObject);
begin
  TComboBox(sender).Items.Clear;
  TComboBox(sender).DropDownCount:=mainform.memscan.getsavedresults(TComboBox(sender).Items)+1;
  TComboBox(sender).Items.Insert(0,'Current scanlist');
end;

function TfrmMemoryViewEx.getCompareMethod: TMVCompareMethod;
//returns the compare method currently selected
begin
  result:=cmOr;
  if rbxor.checked then
    result:=cmxOr
  else
  if rbAnd.checked then
    result:=cmAnd;

end;

procedure TfrmMemoryViewEx.cbCompareChange(Sender: TObject);
begin
  cbSavedList.enabled:=cbCompare.checked;
  rbOr.enabled:=cbCompare.checked;
  rbAnd.enabled:=cbCompare.checked;
  rbXor.enabled:=cbCompare.checked;

  datasource.setcompare(cbCompare.checked, getCompareMethod,  'TMP');
end;

procedure TfrmMemoryViewEx.cbSavedListChange(Sender: TObject);
begin
  if cbSavedList.ItemIndex=0 then
    datasource.setcompare(cbCompare.checked, getCompareMethod, 'TMP')
  else
    datasource.setcompare(cbCompare.checked, getCompareMethod, cbSavedList.text);
end;

procedure TfrmMemoryViewEx.cbColorChange(Sender: TObject);
begin
  {
  Dithered (1 Byte/pixel)
  RGB (3 Bytes/Pixel)
  BGR (3 Bytes/Pixel)
  RGBA (4 Bytes/Pixel)
  BGRA (4 Bytes/Pixel)
  }
  case cbcolor.itemindex of
    0: md.setFormat(GL_COLOR_INDEX);
    1: md.setFormat(GL_RGB);
    2: md.setFormat(GL_BGR);
    3: md.setFormat(GL_RGBA);
    4: md.setFormat(GL_BGRA);
  end;
end;

procedure TfrmMemoryViewEx.cbTypeChange(Sender: TObject);
begin


  {
  Byte
  Unsigned Short
  Short
  Unsigned Int
  Int
  Float
  3-3-2
  2-3-3 R
  5-6-5
  5-6-5 R
  4-4-4-4
  4-4-4-4 R
  5-5-5-1
  1-5-5-5 R
  8-8-8-8
  8-8-8-8 R
  10-10-10-2
  2-10-10-10 R
  }

  case cbtype.ItemIndex of
    0: md.setType(GL_UNSIGNED_BYTE);
    1: md.setType(GL_UNSIGNED_SHORT);
    2: md.setType(GL_SHORT);
    3: md.setType(GL_UNSIGNED_INT);
    4: md.setType(GL_INT);
    5: md.setType(GL_FLOAT);
    6: md.setType(GL_UNSIGNED_BYTE_3_3_2);
    7: md.setType(GL_UNSIGNED_BYTE_2_3_3_REV);
    8: md.setType(GL_UNSIGNED_SHORT_5_6_5);
    9: md.setType(GL_UNSIGNED_SHORT_5_6_5_REV);
    10: md.setType(GL_UNSIGNED_SHORT_4_4_4_4);
    11: md.setType(GL_UNSIGNED_SHORT_4_4_4_4_REV);
    12: md.setType(GL_UNSIGNED_SHORT_5_5_5_1);
    13: md.setType(GL_UNSIGNED_SHORT_1_5_5_5_REV);
    14: md.setType(GL_UNSIGNED_INT_8_8_8_8);
    15: md.setType(GL_UNSIGNED_INT_8_8_8_8_REV);
    16: md.setType(GL_UNSIGNED_INT_10_10_10_2);
    17: md.setType(GL_UNSIGNED_INT_2_10_10_10_REV);
  end;
end;

procedure TfrmMemoryViewEx.Panel1DblClick(Sender: TObject);
var c: tpoint;
    address: ptruint;
begin
  c:=md.ScreenToClient(mouse.cursorpos);
  address:=md.getAddressFromScreenPosition(c.x, c.y);
  MemoryBrowser.hexview.Address:=address;
  MemoryBrowser.show;
end;

procedure TfrmMemoryViewEx.edtPitchChange(Sender: TObject);
var newpitch: integer;
    cb:TNotifyEvent;
    x:integer;
begin
  try
    newpitch:=strtoint(edtpitch.Caption);
    md.setPitch(newpitch);
    edtPitch.Font.Color:=clDefault;
    if(md.fPitch <> newpitch)then
    begin
      cb:=edtPitch.OnChange;
      edtPitch.OnChange:=nil;
      edtPitch.Caption:=IntToStr(md.fPitch);
      edtPitch.OnChange:=cb;
    end;
    MDResize;
    cb:=tbPitch.OnChange;
    tbPitch.OnChange:=nil;
    x:=md.fPitch;
    if(x>tbPitch.Max)then
        x:=tbPitch.Max;
    tbPitch.Position:=x;
    tbPitch.OnChange:=cb;
  except
    edtPitch.Font.Color:=clred;
  end;
end;

procedure TfrmMemoryViewEx.tbPitchChange(Sender: TObject);
var cb:TNotifyEvent;
    pos:integer;
begin
  cb:=edtPitch.OnChange;
  edtPitch.OnChange:=nil;
  pos:=tbPitch.position;
  if((GetKeyState(VK_CONTROL) and $8000) <> 0)then
    pos:=pos*10;
  edtPitch.caption:=inttostr(pos);
  edtPitch.OnChange:=cb;
  md.setPitch(pos);
  MDResize;
end;

procedure TfrmMemoryViewEx.UpdateZoomText(val:single);
begin
  lblZOOM.Caption:=Format('Zoom:%f',[val]);
end;

procedure TfrmMemoryViewEx.UpdateZoomBar(val:single);
var i:integer;
begin
  if(val<=1)then
  begin
    i:=Round(val*256);
    tbZoom.Position:=i
  end
  else
  begin
    i:=Round(((val-1)*8)+256);
    tbZoom.Position:=i;
  end;
end;

procedure TfrmMemoryViewEx.tbZoomChange(Sender: TObject);
var i:integer;
    tmp:single;
begin
    i:=tbZoom.Position;
    if(i>256)then
    begin
        tmp:=i-256;
        tmp:=tmp/8;
        tmp:=tmp+1;
        md.zoom:=tmp;
    end
    else
    begin
        tmp:=i;
        tmp:=tmp/256.0;
        md.zoom:=tmp;
    end;
    UpdateZoomText(md.zoom);
    md.render;
end;

procedure TfrmMemoryViewEx.UpdateScrollbar(val:PtrUInt);
var y:double;
    x:Qword;
    cb:TNotifyEvent;
begin
    if(not Assigned(processhandler))then
      exit;
    x:=$FFFFFFFF;
    if(processhandler.is64Bit)then
      x:=qword(-1);
    y:=val/x;
    y:=Abs(y*sbVERT.Max);
    cb:=sbVERT.OnChange;
    sbVERT.OnChange:=nil;
    sbVERT.Position:=Round(y);
    sbVERT.OnChange:=cb;
end;

procedure TfrmMemoryViewEx.UpdateAddress(val:PtrUInt);
var s:string;
    fmt:string;
begin
  fmt:='%.8X';
  if((val and $FFFFFFFF00000000) <> 0)then
    fmt:='%.16X';
  s:=Format(fmt,[val]);
  edtAddress.Caption:=s;
  edtAddress.SelLength:=0;
end;

procedure TfrmMemoryViewEx.edtAddressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var val:PtrUInt;
begin
  if(key=VK_RETURN)then
  begin
    md.MoveTo(0,0);
    val:=symhandler.getAddressFromName(edtAddress.Text);
    md.setPointer(val);
    UpdateAddress(val);
    UpdateScrollbar(val);
  end;
end;

Procedure TfrmMemoryViewEx.mdMouseDown(Sender:Tobject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var a:PtrUInt;
begin
  if button=mbleft then
  begin
    md.isDragging:=true;
    md.DragOrigin.x:=x;
    md.DragOrigin.y:=y;
    md.PosOrigin.x:=md.fXpos;
    md.PosOrigin.y:=md.fYpos;

    md.dragaddress:=ssCtrl in shift;
    if(md.dragAddress)then
      md.addressOrigin:=md.address;
    a:=md.getAddressFromScreenPosition(0,0);
    UpdateAddress(a);
    UpdateScrollbar(a);
  end;
end;

procedure TfrmMemoryViewEx.mdMouseMove(Sender:Tobject;Shift: TShiftState; X, Y: Integer);
var a: ptruint;
  newp: pointer;
  newsize: integer;
begin
  if(md.isDragging)then
  begin
    with md do
    begin
      if(dragaddress)then
      begin
        //move the address by the difference in X position
        a:=addressOrigin-trunc((PosOrigin.x-(DragOrigin.x-x))/zoom)*fPixelByteSize;
        if assigned(fOnData) and fOnData(a,size,newp,newsize) then
        begin
          address:=a;
          p:=newp;
          size:=newsize;
          LimitCoordinates; //recheck with the new ypos. (in case of size change (end of buf?))
        end;
        render;
      end
      else
        MoveTo(PosOrigin.x-(DragOrigin.x-x), PosOrigin.y+(DragOrigin.y-y));
      a:=md.getAddressFromScreenPosition(0,0);
      UpdateAddress(a);
      UpdateScrollbar(a);
    end;
  end;
end;

procedure TfrmMemoryViewEx.mdMouseUp(Sender:Tobject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button=mbleft then
    md.isDragging:=false;

  //todo: Add a pixel click event handler

  md.render;
end;

procedure TfrmMemoryViewEx.mdMouseWheel(Sender: TObject; Shift: TShiftState;
                                                WheelDelta: Integer;
                                                MousePos: TPoint;
                                                var Handled: Boolean);
var oldx, oldy: single;
  tmp:ptruint;
  val:integer;
  a:PtrUInt;
begin
  with md do
  begin
  if(ssctrl in Shift)then
  begin
    oldx:=(-fXpos+(MousePos.x))/fZoom;
    oldy:=(fYpos+(MousePos.y))/fZoom;
    if WheelDelta>0 then
    begin
      if(fzoom<=32)then
      begin
        //zoom in
        //get the pixel at center of the screen

        if ssShift in Shift then
          fZoom:=fZoom * 1.2
        else
          fZoom:=fZoom * 2;

        fXpos:=trunc(-oldx*fZoom+(MousePos.x));
        fypos:=trunc(oldy*fZoom-(MousePos.y));
      end;
    end
    else if WheelDelta<0 then
    begin
      if(fZoom>0.2)then
      begin
        //zoom out
        if ssShift in Shift then
          fZoom:=fZoom / 1.2
        else
          fZoom:=fZoom / 2;

        fXpos:=trunc(-oldx*fZoom+(MousePos.x));
        fypos:=trunc(oldy*fZoom-(MousePos.y));
      end;
    end;
    UpdateZoomText(fZoom);
    UpdateZoomBar(fZoom);
  end
  else if(ssshift in Shift)then
  begin
    val:=10;
    if(ssRight in shift)then
        val:=1;
    if(WheelDelta<0)then
        setPitch(fPitch-(val*fPixelByteSize))
    else
        setPitch(fPitch+(val*fPixelByteSize));
    edtPitch.Caption:=IntToStr(fPitch);
    tbPitch.Position:=fPitch;
  end
  else
  begin
    val:=1;
    if(ssRight in shift)then
    begin
        val:=4;
        if(ssAlt in shift)then
            val:=40;
    end
    else if(ssAlt in shift)then
        val:=10;
    val:=val*(-WheelDelta);
    fYpos:=fYpos+val;
  end;


  LimitCoordinates;
  setupFont;
  render;
  a:=md.getAddressFromScreenPosition(0,0);
  UpdateAddress(a);
  UpdateScrollbar(a);
  end;
end;

procedure TfrmMemoryViewEx.DisplayHelp;
var s,msg:string;
begin
    msg:='ctrl+wheel=zoom'#13#10+
        'shift+wheel=pitch'#13#10+
        '(+RMB)'#13#10+
        'wheel=scroll'#13#10+
        '(+RMB or +alt)'#13#10+
        'ctrl+click=move address'#13#10;
    s:=Format(#13#10'xpos=%d ypos=%d'#13#10+
                    'pixelsize=%d'#13#10+
                    'size=%.8X'#13#10+
                    'addr=%.8X'#13#10+
                    'width=%d'#13#10+
                    'height=%d'#13#10+
                    'ptr=%p'#13#10,
                    [md.fXpos,md.fYpos,
                    md.fPixelByteSize,
                    md.size,
                    md.address,
                    md.Width,
                    md.Height,
                    md.p]);
    msg:=msg+s;
    MessageBox(self.Handle,PAnsiChar(msg),'MV HELP',MB_OK {$ifdef windows}or MB_SYSTEMMODAL{$endif});
end;

procedure TfrmMemoryViewEx.mdRecenterDrag;
var p: tpoint;
begin
  if(md.isDragging)then
  begin
    p:=self.ScreenToClient(mouse.cursorpos);

    mdMouseDown(self,mbLeft, [], p.x, p.y);
  end;
end;

end.

